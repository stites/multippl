use super::exact::{parse_eexpr, parse_etype};
use super::function::{parse_efunction, parse_sfunction};
use super::sampling::{parse_sexpr, parse_stype};
use super::shared::*;
use crate::typeinf::grammar::Inferable;
use crate::*;
use tree_sitter::{Node, Tree, TreeCursor};

pub fn parse_program(src: &[u8], c: &mut TreeCursor, n: &Node) -> Program<Inferable> {
    let mut c_ = c.clone();
    let mut cs = n.named_children(&mut c_);
    let n = cs.next().unwrap();
    println!("parsing program: {}", n.kind());
    match n.kind() {
        "eexpr" => {
            let e = parse_eexpr(src, c, &n);
            Program::EBody(e)
        }
        "sexpr" => {
            let e = parse_sexpr(src, c, &n);
            Program::SBody(e)
        }
        "efun" => {
            let f = parse_efunction(src, c, &n);
            let n = cs.next().unwrap();
            println!("parsed function: {:?}", f);
            let rest = parse_program(src, c, &n);

            Program::EDefine(f, Box::new(rest))
        }
        "sfun" => {
            let f = parse_sfunction(src, c, &n);
            let n = cs.next().unwrap();
            println!("parsed function: {:?}", f);
            let rest = parse_program(src, c, &n);
            Program::SDefine(f, Box::new(rest))
        }
        s => {
            todo!(
                "\nkind: {}\nsexp: {}\nsrc: {}\n",
                n.kind(),
                n.to_sexp(),
                parse_str(src, &n)
            )
        }
    }
}

pub fn parse_tree(src: &[u8], t: Tree) -> Program<Inferable> {
    // https://docs.rs/tree-sitter/latest/tree_sitter/struct.TreeTreeCursor.html
    let mut c = t.walk();
    let source = c.node();
    assert_eq!(
        source.named_child_count(),
        1,
        "expected one root in {:?}\nsexpr: {}",
        source,
        source.to_sexp(),
    );
    let mut c_ = c.clone();
    let mut cs = source.named_children(&mut c_);
    let root = cs.next().unwrap();
    parse_program(src, &mut c, &root)
}

pub fn parse(code: &str) -> Option<Program<Inferable>> {
    let tree = tree_parser(code.to_string())?;
    let p = parse_tree(code.as_bytes(), tree);
    Some(p)
}
