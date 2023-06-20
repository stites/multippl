use crate::grammar::*;
use crate::typeinf::grammar::*;
use itertools::*;
use rsgm::bayesian_network::BayesianNetwork;
use std::collections::{HashMap, HashSet};

fn is_high(p: &String) -> bool {
    let highs: HashSet<String> =
        HashSet::from_iter(["high", "True", "positive"].into_iter().map(String::from));
    highs.contains(p)
}

fn var_label(var: &String, instantiation: &String) -> String {
    format!("{}_{}", var, instantiation)
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
        .map(|p| Anf::AVar(None, var_label(v, p)))
        .collect_vec();
    let ty = ETy::EProd(params.iter().map(|_| ETy::EBool).collect_vec());
    EExpr::EProd(Some(ty), vec)
}

pub fn ite_star(ifs: &Vec<Anf<Inferable, EVal>>, branches: &Vec<EExprInferable>) -> EExprInferable {
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
        .map(|(parent, passign)| var_label(parent, passign))
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

pub fn compile(bn: &BayesianNetwork, final_query: &EExprInferable) -> EExprInferable {
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

pub fn allmarg_query(bn: &BayesianNetwork) -> EExprInferable {
    let vars = bn.get_variables();
    let mut query = vec![];
    let mut qtype = vec![];
    for var in vars {
        for var_instance in bn.get_all_assignments(var) {
            let qvar = var_label(var, var_instance);
            query.push(Anf::AVar(Some(ETy::EBool), qvar));
            qtype.push(ETy::EBool);
        }
    }
    let t = Some(ETy::EProd(qtype));
    EExpr::EProd(t, query)
}

pub fn compile_allmarg(bn: &BayesianNetwork) -> EExprInferable {
    let query = allmarg_query(&bn);
    compile(&bn, &query)
}
