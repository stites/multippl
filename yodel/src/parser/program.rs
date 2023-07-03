use super::exact::parse_eexpr;
use super::sampling::parse_sexpr;
use super::shared::*;
use crate::typeinf::grammar::Inferable;
use crate::*;
use tree_sitter::{Node, Tree, TreeCursor};

pub fn parse_program(src: &[u8], c: &mut TreeCursor, n: &Node) -> Program<Inferable> {
    use EExpr::*;
    use SExpr::SExact;
    assert_eq!(n.kind(), "program");
    let mut c_ = c.clone();
    let mut cs = n.named_children(&mut c_);
    let n = cs.next().unwrap();
    match n.kind() {
        "eexpr" => {
            let e = parse_eexpr(src, c, &n);
            Program::EBody(e)
        }
        _ => todo!(),
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
