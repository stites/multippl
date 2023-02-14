use grammar;
use std::collections::VecDeque;
use tree_sitter;
use tree_sitter_weight4me;

pub fn parse() -> Option<Tree> {
    let code = r#"
        int double(int x) {
            return x * 2;
        }
    "#;
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_weight4me::language())
        .expect("Error loading weight4me grammar");
    parser.parse(code, None)
}

fn transmogrify_anf(c: &mut Cursor, n: &Node) -> (&mut Cursor, ANF) {
    todo!()
}
fn transmogrify(c: &mut Cursor, n: &Node) -> (&mut Cursor, Expr) {
    let k = n.kind();
    match k {
        "fst" => {
            assert!(n.child_count() == 1);
            let a = n.children(c).first();
            let (c, anf) = transmogrify_any(c, a);
            (c, anf!(anf))
        }
        "snd" => {
            assert!(n.child_count() == 1);
            let a = n.children(c).first();
            let (c, anf) = transmogrify_any(c, a);
            (c, anf!(anf))
        }
        "prod" => {
            assert!(n.child_count() == 2);
            let l = n.children(c).next().unwrap();
            let (c, anf) = transmogrify_any(c, l);

            let r = n.children(c).next().unwrap();
            let (c, anf) = transmogrify_any(c, r);

            assert!(n.children(c).next().is_none());
            (c, anf!(l, r)) // FIXME
        }
        "let_binding" => {
            assert!(n.child_count() == 4);
        }
        "ite_binding" => {
            assert!(n.child_count() == 4);
        }
        "flip" => {
            assert!(n.child_count() == 1);
        }
        "observe" => {
            assert!(n.child_count() == 1);
        }
        "sample" => {
            assert!(n.child_count() == 1);
        }
        "bool" => {
            assert!(n.child_count() == 1);
        }
        "bool_binop" => {
            assert!(n.child_count() == 2);
        }
        "bool_unop" => {
            assert!(n.child_count() == 1);
        }
        "float" => {
            assert!(n.child_count() == 1);
            n.children()
        }
        "float_op" => {
            assert!(n.child_count() == 2);
        }
        "anf" => {
            assert!(n.child_count() == 1);
        }
        _ => panic!(
            "unexpected tree-sitter node kind! Likely, you need to rebuild the tree-sitter parser"
        ),
    }
}
pub fn elaborate(t: Tree) -> Expr {
    // https://docs.rs/tree-sitter/latest/tree_sitter/struct.TreeCursor.html
    let cursor = t.walk();
    transmogrify(cursor, cursor.node())
}
