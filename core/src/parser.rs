// use itertools::Itertools;
// use std::collections::VecDeque;
// use tree_sitter::*;
// use tree_sitter_weight4me;

// use crate::grammar::*;

// pub fn parse(code: String) -> Option<Tree> {
//     let mut parser = Parser::new();
//     parser
//         .set_language(tree_sitter_weight4me::language())
//         .expect("Error loading weight4me grammar");
//     parser.parse(code, None)
// }

// // fn transmogrify_anf<'a, 'b>(c: &'a mut TreeCursor<'b>, n: &'b Node) -> (ANF, Ty) {
// fn transmogrify_anf(c: &mut TreeCursor, n: Node) -> (ANF, Ty) {
//     todo!()
// }

// macro_rules! assert_children {
//     ( $x:expr, $count:literal, $node:expr, $c:expr ) => {{
//         let mut c__ = $c.clone();
//         let cs = $node.named_children(&mut c__).into_iter().collect_vec();
//         assert!(
//             $node.named_child_count() == $count,
//             "{} #named_children: {}\nchildren: {:?}\nsexp: {}\n",
//             $x,
//             $node.named_child_count(),
//             cs,
//             $node.to_sexp()
//         );
//     }};
// }

// fn transmogrify(c: &mut TreeCursor, n: &Node) -> Expr {
//     // fn transmogrify<'a, 'b>(c: &'a mut TreeCursor<'b>, n: &'b Node) -> Expr {
//     let k = n.kind();
//     match k {
//         "fst" => {
//             assert_children!(k, 1, n, c);
//             let mut _c = c.clone();
//             let mut cs = n.named_children(&mut _c);
//             let a = cs.next().unwrap();
//             let (anf, ty) = transmogrify_anf(c, a);
//             return Expr::EAnf(Box::new(anf));
//         }
//         "snd" => {
//             assert!(n.named_child_count() == 1, "{k} #named_children: {}, ", n.named_child_count());
//             let mut _c = c.clone();
//             let mut cs = n.named_children(&mut _c);
//             let a = cs.next().unwrap();
//             let (anf, ty) = transmogrify_anf(c, a);
//             Expr::EAnf(Box::new(anf))
//         }
//         "prod" => {
//             assert!(n.named_child_count() == 2, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
//             let mut _c = c.clone();
//             let mut cs = n.named_children(&mut _c);
//             let l = cs.next().unwrap();
//             let (l, tyl) = transmogrify_anf(c, l);

//             let r = cs.next().unwrap();
//             let (r, tyr) = transmogrify_anf(c, r);

//             assert!(cs.next().is_none(), "{k}");
//             // Expr::EProd(
//             //     Box::new(l),
//             //     Box::new(r),
//             //     Box::new(Ty::Prod(Box::new(tyl), Box::new(tyr))),
//             // )
//             todo!()
//         }
//         "let_binding" => {
//             // assert!(n.named_child_count() == 4, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
//             assert_children!(k, 4, n, c);
//             let mut _c = c.clone();
//             let mut cs = n.named_children(&mut _c);
//             let l = cs.next(); println!("{l:?}");
//             let l = cs.next(); println!("{l:?}");
//             let l = cs.next(); println!("{l:?}");
//             let l = cs.next(); println!("{l:?}");
//             let l = cs.next(); println!("{l:?}");
//             let l = cs.next(); println!("{l:?}");
//             let l = cs.next(); println!("{l:?}");
//             let l = cs.next(); println!("{l:?}");

//             todo!()
//         }
//         "ite_binding" => {
//             assert!(n.named_child_count() == 4, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
//             todo!()
//         }
//         "flip" => {
//             assert!(n.named_child_count() == 1, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
//             todo!()
//         }
//         "observe" => {
//             assert!(n.named_child_count() == 1, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
//             todo!()
//         }
//         "sample" => {
//             assert!(n.named_child_count() == 1, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
//             todo!()
//         }
//         "bool" => {
//             assert!(n.named_child_count() == 1, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
//             todo!()
//         }
//         "bool_binop" => {
//             assert!(n.named_child_count() == 2, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
//             todo!()
//         }
//         "bool_unop" => {
//             assert!(n.named_child_count() == 1, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
//             todo!()
//         }
//         "float" => {
//             assert!(n.named_child_count() == 1, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
//             todo!()
//         }
//         "float_op" => {
//             assert!(n.named_child_count() == 2, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
//             todo!()
//         }
//         "anf" => {
//             assert!(n.named_child_count() == 1, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
//             todo!()
//         }
//         s => panic!(
//             "unexpected tree-sitter node kind `{}` (#named_children: {})! Likely, you need to rebuild the tree-sitter parser\nsexp: {}", s, n.named_child_count(), n.to_sexp()
//         ),
//     }
// }
// pub fn elaborate(t: Tree) -> Expr {
//     // https://docs.rs/tree-sitter/latest/tree_sitter/struct.TreeTreeCursor.html
//     let mut c = t.walk();
//     let source = c.node();
//     assert!(source.named_child_count() == 1);
//     let mut c_ = c.clone();
//     let mut cs = source.named_children(&mut c_);
//     let root = cs.next().unwrap();
//     let e = transmogrify(&mut c, &root);
//     e
// }

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
