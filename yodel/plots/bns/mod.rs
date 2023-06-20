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

fn is_high(p: &String) -> bool {
    let highs: HashSet<String> =
        HashSet::from_iter(["high", "True", "positive"].into_iter().map(String::from));
    highs.contains(p)
}

fn get_probs(
    bn: &BayesianNetwork,
    v: &String,
    parents: &HashMap<String, String>,
) -> (Vec<f64>, Vec<String>) {
    bn.get_all_assignments(v)
        .into_iter()
        .cloned()
        .map(|vassign| (bn.get_conditional_prob(v, &vassign, parents), vassign))
        .unzip()
}

fn _to_statements(
    bn: &BayesianNetwork,
    v: &String,
    parents: &HashMap<String, String>,
) -> (Vec<(String, EExprInferable)>, Vec<String>) {
    let (mut ps, mut vars) = get_probs(bn, v, parents);
    if ps.len() == 2 && !is_high(vars.first().unwrap()) {
        ps.rotate_right(1);
        vars.rotate_right(1);
    }
    (discrete::params2named_statements(&v, &vars, &ps), vars)
}

fn to_statements(
    bn: &BayesianNetwork,
    v: &String,
    parents: &HashMap<String, String>,
) -> Vec<(String, EExprInferable)> {
    _to_statements(bn, v, parents).0
}

fn to_statements_returning(
    bn: &BayesianNetwork,
    v: &String,
    parents: &HashMap<String, String>,
) -> (Vec<(String, EExprInferable)>, EExprInferable) {
    let (lbl_binds, vars) = _to_statements(bn, v, parents);
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
    assert_eq!(
        ifs.len() + 1,
        branches.len(),
        "must have (#ifs + 1else) = #branches"
    );
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
fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let bn = BayesianNetwork::from_string(std::fs::read_to_string(&args.file).unwrap().as_str());
    print_network!(bn);
    println!("----------------------------");

    for v in bn.topological_sort() {
        let assigns = bn.get_all_assignments(&v);
        let parents = bn.get_parents(&v);
        if v == String::from("Dyspnoea") {
            continue;
        }
        if parents.len() == 0 {
            for (label, stmnt) in to_statements(&bn, &v, &Default::default()) {
                println!("let {} \t= {:?}", label, stmnt);
            }
        }
        if parents.len() == 1 {
            let all_passigns = bn.parent_assignments(&v);
            let last_branch_ix = all_passigns.len() - 1;
            let mut consistent_return = None;

            println!("let prod = ");
            for (ix, passigns) in all_passigns.into_iter().enumerate() {
                let pvar = parent_vars(&passigns);
                if ix != last_branch_ix {
                    println!("    if {:?}", conjoin_vars(&pvar));
                } else {
                    println!("    else");
                }
                let (l_s, vars) = _to_statements(&bn, &v, &passigns);
                let query = vars2prod(&v, &vars);
                consistent_return = Some(query.clone());

                for (label, stmnt) in l_s {
                    println!("        let {} \t= {:?}", label, stmnt);
                }
                println!("        in {:?}", query);
            }
            for (l, prj) in prod2vars(&String::from("prod"), &consistent_return.unwrap()) {
                println!("let {} in {:?}", l, prj);
            }
        }
        if parents.len() == 2 {
            let all_passigns = bn.parent_assignments(&v);
            let last_branch_ix = all_passigns.len() - 1;
            let mut consistent_return = None;

            println!("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<");
            println!("let prod = ");
            for (ix, passigns) in all_passigns.into_iter().enumerate() {
                let pvar = parent_vars(&passigns);
                if ix != last_branch_ix {
                    println!("    if {:?}", conjoin_vars(&pvar));
                } else {
                    println!("    else");
                }
                let (l_s, vars) = _to_statements(&bn, &v, &passigns);
                let query = vars2prod(&v, &vars);
                consistent_return = Some(query.clone());

                for (label, stmnt) in l_s {
                    println!("        let {} \t= {:?}", label, stmnt);
                }
                println!("        in {:?}", query);
                println!("");
            }
            for (l, prj) in prod2vars(&String::from("prod"), &consistent_return.unwrap()) {
                println!("let {} in {:?}", l, prj);
            }

            println!(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
        }
        println!("");
    }
    Ok(())
}
