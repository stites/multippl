use tree_sitter::*;
use tree_sitter_formula;

pub enum Formula {
    Var(String),
    Neg(Box<Formula>),
    And(Box<Formula>, Box<Formula>),
    Or(Box<Formula>, Box<Formula>),
}

pub fn parse(code: String) -> Option<Tree> {
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_formula::language())
        .expect("Error loading formula grammar");
    parser.parse(code, None)
}

pub fn elaborate(t: Tree) -> Formula {
    // https://docs.rs/tree-sitter/latest/tree_sitter/struct.TreeTreeCursor.html
    let mut c = t.walk();
    let source = c.node();
    let mut c_ = c.clone();
    let mut cs = source.named_children(&mut c_);
    let root = cs.next().unwrap();
    let e = as_ast(&mut c, &root);
    e
}

fn as_ast(_c: &mut TreeCursor, n: &Node) -> Formula {
    let k = n.kind();
    match k {
        "fst" => {
            todo!()
        }
        "snd" => {
            todo!()
        }
        "prod" => {
            todo!()
                    }
        "let_binding" => {
            todo!()
        }
        s => panic!(
            "unexpected tree-sitter node kind `{}` (#named_children: {})! Likely, you need to rebuild the tree-sitter parser\nsexp: {}", s, n.named_child_count(), n.to_sexp()
        ),
    }
}
// #[cfg(test)]
// mod parser_tests {
//     use super::*;
//     use crate::*;
//     use std::any::TypeId;

//     /// ======================
//     /// exact: trivial
//     /// ======================
//     ///
//     /// let x = true in
//     /// x
//     ///
//     /// ---
//     ///
//     /// (source_file
//     ///   (let_binding
//     ///     (identifier)
//     ///     (anf (bool))
//     ///   (anf (identifier))))
//     #[test]
//     #[ignore = "punt on parsing"]
//     fn one_var() {
//         let code = r#"let x = true in x"#;
//         let tree = parse(code.to_string());
//         assert!(tree.is_some());
//         let tree = tree.unwrap();
//         let expr = elaborate(tree);
//         assert!(expr == lets!["x" : bool := b!("true"); in var!("x") ; bool]);
//     }
// }
