use crate::grammar::*;
use crate::typeinf::grammar::*;
use rsgm::bayesian_network::BayesianNetwork;
use std::collections::{HashMap, HashSet};

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
                let show_passigns: Vec<_> = passigns
                    .iter()
                    .map(|(p, pa)| format!("{}={}", p, pa))
                    .collect();

                for vassign in $bn.get_all_assignments(&v) {
                    println!(
                        "            P({} = {}\t| {})\t: {}",
                        v,
                        vassign,
                        show_passigns.join(", "),
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
        _ => todo!(),
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
