use crate::typeinf::grammar::{AnfInferable, ExprInferable, ProgramInferable};
use itertools::Itertools;
use std::collections::VecDeque;
use tree_sitter::*;
use tree_sitter_yodel;

use crate::grammar::*;

pub fn tree_parser(code: String) -> Option<Tree> {
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_yodel::language())
        .expect("Error loading yodel grammar");
    parser.parse(code, None)
}

// fn parse_anf<'a, 'b>(c: &'a mut TreeCursor<'b>, n: &'b Node) -> (ANF, Ty) {
fn parse_anf(c: &mut TreeCursor, n: Node) -> (AnfInferable, Ty) {
    todo!()
}

macro_rules! assert_children {
    ( $x:expr, $count:literal, $node:expr, $c:expr ) => {{
        let mut c__ = $c.clone();
        let cs = $node.named_children(&mut c__).into_iter().collect_vec();
        assert!(
            $node.named_child_count() == $count,
            "{} #named_children: {}\nchildren: {:?}\nsexp: {}\n",
            $x,
            $node.named_child_count(),
            cs,
            $node.to_sexp()
        );
    }};
}

fn parse_expr(c: &mut TreeCursor, n: &Node) -> ExprInferable {
    let k = n.kind();
    match k {
        // values
        "bool" => {
            assert!(n.named_child_count() == 1, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
            todo!()
        }
        "bool_binop" => {
            assert!(n.named_child_count() == 2, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
            todo!()
        }
        "bool_unop" => {
            assert!(n.named_child_count() == 1, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
            todo!()
        }
        "float" => {
            assert!(n.named_child_count() == 1, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
            todo!()
        }
        "float_op" => {
            assert!(n.named_child_count() == 2, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
            todo!()
        }
        "anf" => {
            assert!(n.named_child_count() == 1, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
            todo!()
        }
        // finished with Anf expressions
        "fst" => {
            assert_children!(k, 1, n, c);
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);
            let a = cs.next().unwrap();
            let (anf, ty) = parse_anf(c, a);
            return Expr::EAnf((), Box::new(anf));
        }
        "snd" => {
            assert!(n.named_child_count() == 1, "{k} #named_children: {}, ", n.named_child_count());
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);
            let a = cs.next().unwrap();
            let (anf, ty) = parse_anf(c, a);
            Expr::EAnf((), Box::new(anf))
        }
        // EPrj(<EPrjExt as ξ<X>>::Ext, usize, Box<Anf<X>>),
        "prod" => {
            assert!(n.named_child_count() == 2, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);
            let l = cs.next().unwrap();
            let (l, tyl) = parse_anf(c, l);

            let r = cs.next().unwrap();
            let (r, tyr) = parse_anf(c, r);

            assert!(cs.next().is_none(), "{k}");
            // Expr::EProd(
            //     Box::new(l),
            //     Box::new(r),
            //     Box::new(Ty::Prod(Box::new(tyl), Box::new(tyr))),
            // )
            todo!()
        }
        "let_binding" => {
            // assert!(n.named_child_count() == 4, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
            assert_children!(k, 4, n, c);
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);
            let l = cs.next(); println!("{l:?}");
            let l = cs.next(); println!("{l:?}");
            let l = cs.next(); println!("{l:?}");
            let l = cs.next(); println!("{l:?}");
            let l = cs.next(); println!("{l:?}");
            let l = cs.next(); println!("{l:?}");
            let l = cs.next(); println!("{l:?}");
            let l = cs.next(); println!("{l:?}");

            todo!()
        }
        "ite_binding" => {
            assert!(n.named_child_count() == 4, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
            todo!()
        }
        "flip" => {
            assert!(n.named_child_count() == 1, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
            todo!()
        }
        "observe" => {
            assert!(n.named_child_count() == 1, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
            todo!()
        }
        "sample" => {
            assert!(n.named_child_count() == 1, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
            todo!()
        }
        s => panic!(
            "unexpected tree-sitter node kind `{}` (#named_children: {})! Likely, you need to rebuild the tree-sitter parser\nsexp: {}", s, n.named_child_count(), n.to_sexp()
        ),
    }
}

fn parse_program(c: &mut TreeCursor, n: &Node) -> ProgramInferable {
    Program::Body(parse_expr(c, n))
}
pub fn parse_tree(t: Tree) -> ProgramInferable {
    // https://docs.rs/tree-sitter/latest/tree_sitter/struct.TreeTreeCursor.html
    let mut c = t.walk();
    let source = c.node();
    assert_eq!(
        source.named_child_count(),
        1,
        "expected one root in {:?}",
        source
    );
    let mut c_ = c.clone();
    let mut cs = source.named_children(&mut c_);
    let root = cs.next().unwrap();
    parse_program(&mut c, &root)
}

#[cfg(test)]
mod parser_tests {
    use super::*;
    use crate::*;
    use std::any::TypeId;

    /// ======================
    /// exact: trivial
    /// ======================
    ///
    /// let x = true in
    /// x
    ///
    /// ---
    ///
    /// (source_file
    ///   (let_binding
    ///     (identifier)
    ///     (anf (bool))
    ///   (anf (identifier))))
    #[test]
    fn one_var() {
        let code = r#"let x = true in x"#;
        let tree = tree_parser(code.to_string());
        assert!(tree.is_some());
        let tree = tree.unwrap();
        let expr = parse_tree(tree);
        assert_eq!(expr, program!(lets!["x" ;= b!("true"); in var!("x")]));
    }
}
