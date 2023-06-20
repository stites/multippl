use clap::Parser;
use itertools::*;
use rsgm::bayesian_network::BayesianNetwork;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use yodel::grammar::*;
use yodel::typeinf::grammar::*;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// An input Bayesian network file in JSON format
    #[clap(short, long, value_parser)]
    file: String,
}

#[macro_export]
macro_rules! print_network {
    ( $bn:expr ) => {{
        println!("{:?}", $bn);
        println!("");
        println!("{:?}", $bn.topological_sort());

        for v in $bn.topological_sort() {
            println!("{}", v);
            println!("    assignments: {}", $bn.get_all_assignments(&v).join(","));
            println!("    parents    : {}", $bn.get_parents(&v).join(","));
            println!("    CPT");
            for passigns in $bn.parent_assignments(&v) {
                let show_passigns = passigns
                    .iter()
                    .map(|(p, pa)| format!("{}={}", p, pa))
                    .collect_vec()
                    .join(", ");
                for vassign in $bn.get_all_assignments(&v) {
                    println!(
                        "            P({} = {}\t| {})\t: {}",
                        v,
                        vassign,
                        show_passigns,
                        $bn.get_conditional_prob(&v, &vassign, &passigns)
                    );
                }
            }
        }
    }};
}

pub fn _pprint_anf(anf: &Anf<Inferable, EVal>) {
    match &anf {
        Anf::AVal(_, val) => {
            print!("{:?}", val);
        }
        Anf::AVar(_, var) => {
            print!("{}", var);
        }
        Anf::Neg(p) => {
            print!("!");
            _pprint_anf(p);
        }
        Anf::Or(l, r) => {
            _pprint_anf(l);
            print!(" && ");
            _pprint_anf(r);
        }
        Anf::And(l, r) => {
            _pprint_anf(l);
            print!(" && ");
            _pprint_anf(r);
        }
    }
}

pub fn is_let(expr: &EExprInferable) -> bool {
    match &expr {
        EExpr::ELetIn(_, var, bindee, body) => true,
        _ => false,
    }
}
pub fn _pprint(expr: &EExprInferable, depth: usize) {
    match &expr {
        EExpr::EFlip(_, p) => {
            print!("flip {}", p);
        }
        EExpr::EAnf(_, anf) => {
            _pprint_anf(anf);
        }
        EExpr::EPrj(_, ix, anf) => {
            print!("prj_{}(", ix);
            _pprint_anf(anf);
            print!(")");
        }
        EExpr::EProd(_, anfs) => {
            for anf in anfs {
                _pprint_anf(anf);
                print!(", ");
            }
        }
        EExpr::ELetIn(_, var, bindee, body) => {
            let indent = " ".repeat(depth * 2);
            print!("{}let {} := ", indent, var);
            _pprint(bindee, depth);
            println!(" in");
            print!("{}", indent);
            _pprint(body, depth);
            println!("");
        }
        EExpr::EIte(_, p, t, f) => {
            let indent = " ".repeat((depth + 2) * 2);
            print!("\n{}if ", indent);
            _pprint_anf(p);
            println!(" ");
            println!("{}then", indent);
            _pprint(t, depth + 2);
            println!("{}else", indent);
            _pprint(f, depth + 2);
        }
        x => {
            let indent = " ".repeat(depth * 2);
            println!("{}{:?}", indent, x)
        }
        _ => println!(""),
    }
}

pub fn pprint(x: &EExprInferable) {
    _pprint(x, 0)
}

fn is_high(p: &String) -> bool {
    let highs: HashSet<String> =
        HashSet::from_iter(["high", "True", "positive"].into_iter().map(String::from));
    highs.contains(p)
}

fn get_probs(
    bn: &BayesianNetwork,
    v: &String,
    parents: &HashMap<String, String>,
    suffix: Option<String>,
) -> (Vec<f64>, Vec<String>) {
    bn.get_all_assignments(v)
        .into_iter()
        .cloned()
        .map(|vassign| {
            (
                bn.get_conditional_prob(v, &vassign, parents),
                format!("{}{}", vassign, suffix.clone().unwrap_or_default()),
            )
        })
        .unzip()
}

fn _to_statements(
    bn: &BayesianNetwork,
    v: &String,
    parents: &HashMap<String, String>,
    suffix: Option<String>,
) -> (Vec<(String, EExprInferable)>, Vec<String>) {
    let (mut ps, mut vars) = get_probs(bn, v, parents, suffix);
    if ps.len() == 2 && !is_high(vars.first().unwrap()) {
        ps.rotate_right(1);
        vars.rotate_right(1);
    }
    (discrete::params2named_statements(&v, &vars, &ps), vars)
}

fn independent_node(bn: &BayesianNetwork, v: &String) -> Vec<(String, EExprInferable)> {
    _to_statements(bn, v, &Default::default(), None).0
}

fn to_statements_returning(
    bn: &BayesianNetwork,
    v: &String,
    parents: &HashMap<String, String>,
    suffix: Option<String>,
) -> (Vec<(String, EExprInferable)>, EExprInferable) {
    let (lbl_binds, vars) = _to_statements(bn, v, parents, suffix);
    let body = vars2prod(v, &vars);
    (lbl_binds, body)
}

fn vars2prod(v: &String, params: &Vec<String>) -> EExprInferable {
    let vec = params
        .iter()
        .map(|p| Anf::AVar(None, format!("{}_{}", v, p)))
        .collect_vec();
    let ty = ETy::EProd(params.iter().map(|_| ETy::EBool).collect_vec());
    EExpr::EProd(Some(ty), vec)
}

fn ite_star(ifs: &Vec<Anf<Inferable, EVal>>, branches: &Vec<EExprInferable>) -> EExprInferable {
    let mut ifs = ifs.clone();
    if ifs.len() == branches.len() {
        ifs.pop();
    }

    let mut branches = branches.clone();
    let mut ite = branches.pop().unwrap();
    for (p, doit) in ifs.iter().zip(branches.iter()).rev() {
        ite = EExpr::EIte(
            None,
            Box::new(p.clone()),
            Box::new(doit.clone()),
            Box::new(ite),
        );
    }
    ite
}

fn prod2vars(name: &String, p: &EExprInferable) -> Vec<(String, EExprInferable)> {
    match p {
        EExpr::EProd(_, prod) => prod
            .into_iter()
            .enumerate()
            .map(|(ix, anf)| match anf {
                Anf::AVar(_, inst) => (
                    inst.clone(),
                    EExpr::EPrj(
                        Some(ETy::EBool),
                        ix,
                        Box::new(Anf::AVar(None, name.clone())),
                    ),
                ),
                _ => panic!("stop that!"),
            })
            .collect_vec(),
        _ => panic!("bad user! go away!"),
    }
}

fn parent_vars(passigns: &HashMap<String, String>) -> Vec<String> {
    passigns
        .iter()
        .map(|(parent, passign)| format!("{}_{}", parent, passign))
        .collect()
}

fn conjoin_vars(vars: &Vec<String>) -> Anf<Inferable, EVal> {
    let mut vars = vars.clone();
    vars.rotate_left(1);
    let hd = vars.pop().unwrap();

    let mut fin = Box::new(Anf::AVar(None, hd));
    for v in vars {
        fin = Box::new(Anf::And(fin, Box::new(Anf::AVar(None, v))));
    }
    *fin
}

fn compile_network(bn: &BayesianNetwork, final_query: &EExprInferable) -> EExprInferable {
    let mut program = final_query.clone();
    for (node_ix, v) in bn.topological_sort().into_iter().enumerate().rev() {
        let assigns = bn.get_all_assignments(&v);
        let parents = bn.get_parents(&v);
        if parents.len() == 0 {
            for (label, stmnt) in independent_node(&bn, &v).into_iter().rev() {
                program = EExpr::ELetIn(None, label, Box::new(stmnt), Box::new(program));
            }
        }
        if parents.len() > 0 {
            let all_passigns = bn.parent_assignments(&v);
            let last_branch_ix = all_passigns.len() - 1;
            let mut consistent_return = None;
            let mut guards = vec![];
            let mut branches = vec![];
            for (pix, passigns) in all_passigns.into_iter().enumerate() {
                if consistent_return.is_none() {
                    let vars = _to_statements(&bn, &v, &passigns, None).1;
                    let query = vars2prod(&v, &vars);
                    consistent_return = Some(query.clone());
                }
                let pvar = parent_vars(&passigns);

                guards.push(conjoin_vars(&pvar));
                let (l_s, vars) = _to_statements(&bn, &v, &passigns, Some(format!("_{}", pix)));
                let query = vars2prod(&v, &vars);

                let mut branch = query;
                for (label, stmnt) in l_s.into_iter().rev() {
                    branch = EExpr::ELetIn(None, label, Box::new(stmnt), Box::new(branch));
                }
                branches.push(branch);
            }
            let prod = ite_star(&guards, &branches);

            for (l, prj) in prod2vars(&format!("node_{}", node_ix), &consistent_return.unwrap()) {
                program = EExpr::ELetIn(None, l, Box::new(prj), Box::new(program));
            }
            program = EExpr::ELetIn(
                None,
                format!("node_{}", node_ix),
                Box::new(prod),
                Box::new(program),
            );
        }
    }
    program
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let bn = BayesianNetwork::from_string(std::fs::read_to_string(&args.file).unwrap().as_str());
    print_network!(bn);
    println!("----------------------------");
    let final_query = EExpr::EAnf((), Box::new(Anf::AVar(None, String::from("final_query"))));
    let program = compile_network(&bn, &final_query);
    pprint(&program);
    Ok(())
}
