#![allow(dead_code)]
use rsdd::builder::bdd_builder::*;
use rsdd::builder::cache::all_app::AllTable;
use std::char::decode_utf16;
use std::collections::HashMap;
use tree_sitter::*;

#[derive(Debug, PartialEq, Eq)]
pub enum Formula {
    Var(String),
    Neg(Box<Formula>),
    And(Box<Formula>, Box<Formula>),
    Or(Box<Formula>, Box<Formula>),
}

pub fn parse(src: String) -> Option<Tree> {
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_formula::language())
        .expect("Error loading formula grammar");
    parser.parse(src, None)
}

pub fn elaborate(src: String, t: Tree) -> Result<Formula, String> {
    // https://docs.rs/tree-sitter/latest/tree_sitter/struct.TreeTreeCursor.html
    let mut c = t.walk();
    let source = c.node();
    let mut cs = source.named_children(&mut c);
    let root = cs.next().unwrap();
    let src: Vec<u16> = src.encode_utf16().collect();
    as_ast(&src, &root)
}

fn get_var(src: &[u16], n: &Node) -> Result<Formula, String> {
    let bytestr = n.utf16_text(src);
    let cs: String = decode_utf16(bytestr.to_vec())
        .map(|r| r.unwrap_or(std::char::REPLACEMENT_CHARACTER))
        .collect();
    Ok(Formula::Var(cs))
}

fn as_ast(src: &[u16], n: &Node) -> Result<Formula, String> {
    let k = n.kind();
    match k {
        "var" => get_var(src, n),
        "neg" => {
            let mut c = n.walk();
            let cs : Vec<_> = n.named_children(&mut c).collect();
            assert_eq!(cs.len(),1);
            let f = as_ast(src, &cs[0])?;
            Ok(Formula::Neg(Box::new(f)))
        },
        "and" => {
            let mut c = n.walk();
            let cs : Vec<_> = n.named_children(&mut c).collect();
            assert_eq!(cs.len(),2);
            let l = as_ast(src, &cs[0])?;
            let r = as_ast(src, &cs[1])?;
            Ok(Formula::And(Box::new(l), Box::new(r)))
        },
        "or" => {
            let mut c = n.walk();
            let cs : Vec<_> = n.named_children(&mut c).collect();
            assert_eq!(cs.len(),2);
            let l = as_ast(src, &cs[0])?;
            let r = as_ast(src, &cs[1])?;
            Ok(Formula::Or(Box::new(l), Box::new(r)))
        },
        s => panic!(
            "unexpected tree-sitter node kind `{}` (#named_children: {})! Likely, you need to rebuild the tree-sitter parser\nsexp: {}", s, n.named_child_count(), n.to_sexp()
        ),
    }
}
pub struct Context<'a, T: LruTable<BddPtr>> {
    pub mgr: &'a mut BddManager<T>,
    pub names: HashMap<String, VarLabel>,
}

impl<'a, T: LruTable<BddPtr>> Context<'a, T> {
    fn new(mgr: &'a mut BddManager<T>) -> Self {
        Self {
            mgr,
            names: Default::default(),
        }
    }
}

pub struct Output {
    pub circuit: BddPtr,
    pub variables: HashMap<VarLabel, BddPtr>,
    pub names: HashMap<String, VarLabel>,
}

fn compile<T: LruTable<BddPtr>>(ctx: &mut Context<T>, f: &Formula) -> Result<Output, String> {
    use Formula::*;
    match f {
        Var(s) => match ctx.names.get(s) {
            None => {
                let (l, p) = ctx.mgr.new_pos();
                let variables = HashMap::from([(l, p)]);
                let names = HashMap::from([(s.clone(), l)]);
                Ok(Output {
                    circuit: p,
                    variables,
                    names,
                })
            }
            Some(l) => {
                let p = ctx.mgr.var(*l, true);
                let variables = HashMap::from([(*l, p)]);
                Ok(Output {
                    circuit: p,
                    variables,
                    names: ctx.names.clone(),
                })
            }
        },
        Neg(f) => {
            let o = compile(ctx, f)?;
            Ok(Output {
                circuit: o.circuit.neg(),
                variables: o.variables,
                names: o.names,
            })
        }
        And(l, r) => {
            let l = compile(ctx, l)?;
            let r = compile(ctx, r)?;
            let circuit = ctx.mgr.and(l.circuit, r.circuit);
            let mut variables = l.variables;
            variables.extend(r.variables);
            let mut names = l.names;
            names.extend(r.names);
            Ok(Output {
                circuit,
                variables,
                names,
            })
        }
        Or(l, r) => {
            let l = compile(ctx, l)?;
            let r = compile(ctx, r)?;
            let circuit = ctx.mgr.or(l.circuit, r.circuit);
            let mut variables = l.variables;
            variables.extend(r.variables);
            let mut names = l.names;
            names.extend(r.names);
            Ok(Output {
                circuit,
                variables,
                names,
            })
        }
    }
}

pub fn eval(src: String) -> Result<(Output, BddManager<AllTable<BddPtr>>), String> {
    let mgr = BddManager::<AllTable<BddPtr>>::new_default_order(0);
    let names = Default::default();
    eval_with(src, mgr, names)
}

pub fn eval_with(
    src: String,
    mgr: BddManager<AllTable<BddPtr>>,
    names: HashMap<String, VarLabel>,
) -> Result<(Output, BddManager<AllTable<BddPtr>>), String> {
    let tree = parse(src.to_string());
    let formula = elaborate(src, tree.unwrap())?;

    let mut mgr = mgr;
    let mut ctx = Context {
        mgr: &mut mgr,
        names,
    };
    let out = compile(&mut ctx, &formula)?;
    Ok((out, mgr))
}

#[cfg(test)]
mod parser_tests {
    use super::*;
    use crate::*;
    use tracing_test::traced_test;
    use Formula::*;

    #[test]
    #[traced_test]
    fn test_var() {
        let src = r#"xyx302"#;
        let tree = parse(src.to_string());
        assert!(tree.is_some());
        let tree = tree.unwrap();
        let expr = elaborate(src.to_string(), tree);
        assert!(expr == Ok(Formula::Var("xyx302".to_string())));
    }

    #[test]
    #[traced_test]
    fn test_neg() {
        let src = r#"!xyx302"#;
        let tree = parse(src.to_string());
        assert!(tree.is_some());
        let tree = tree.unwrap();
        let expr = elaborate(src.to_string(), tree);
        assert_eq!(expr, Ok(Neg(Box::new(Var("xyx302".to_string())))));
    }

    #[test]
    #[traced_test]
    fn test_and() {
        let src = r#"(xyx302 & abc)"#;
        let tree = parse(src.to_string());
        assert!(tree.is_some());
        let tree = tree.unwrap();
        let expr = elaborate(src.to_string(), tree);
        assert_eq!(
            expr,
            Ok(And(
                Box::new(Var("xyx302".to_string())),
                Box::new(Var("abc".to_string()))
            ))
        );
    }

    #[test]
    #[traced_test]
    fn test_or() {
        let src = r#"(xyx302 | abc)"#;
        let tree = parse(src.to_string());
        assert!(tree.is_some());
        let tree = tree.unwrap();
        let expr = elaborate(src.to_string(), tree);
        assert_eq!(
            expr,
            Ok(Or(
                Box::new(Var("xyx302".to_string())),
                Box::new(Var("abc".to_string()))
            ))
        );
    }
}
