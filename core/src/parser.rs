use crate::typeinf::grammar::{AnfInferable, ExprInferable, ProgramInferable};
use itertools::Itertools;
use std::collections::VecDeque;
use tree_sitter::*;
use tree_sitter_yodel;

use crate::grammar::*;

macro_rules! assert_children {
    ( $x:expr, $count:literal, $node:expr, $c:expr ) => {{
        let mut c__ = $c.clone();
        let cs = $node.named_children(&mut c__).into_iter().collect_vec();
        assert!(
            $node.named_child_count() == $count,
            "{} #named_children: {} (expected {})\nchildren: {:?}\nsexp: {}\n",
            $x,
            $node.named_child_count(),
            $count,
            cs,
            $node.to_sexp()
        );
    }};
}

pub fn tree_parser(code: String) -> Option<Tree> {
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_yodel::language())
        .expect("Error loading yodel grammar");
    parser.parse(code, None)
}

// fn parse_anf<'a, 'b>(c: &'a mut TreeCursor<'b>, n: &'b Node) -> (ANF, Ty) {
// anf: $ => choice(
//   $.identifier,
//   $._value,
//   prec.left(3, seq($.anf, $.bool_biop, $.anf)),
//   prec.left(5, seq($.bool_unop, $.anf)),
// ),

fn parse_anf_child_h(src: &[u8], c: &mut TreeCursor, n: Node) -> AnfInferable {
    let mut c_ = c.clone();
    let mut cs = n.named_children(&mut c_);
    let a = cs.next().unwrap();
    parse_anf(src, c, a)
}
fn parse_anf(src: &[u8], c: &mut TreeCursor, n: Node) -> AnfInferable {
    match n.named_child_count() {
        0 => parse_anf_node(src, c, n),
        1 => parse_anf_child_h(src, c, n),
        2 => {
            // unary operation
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);
            let op = cs.next().unwrap();
            let utf8 = op.utf8_text(src).unwrap();
            let op = String::from_utf8(utf8.into()).unwrap();
            assert_eq!(
                op,
                "!".to_string(),
                "invalid program found!\nsexp: {}",
                n.to_sexp()
            );
            let a = cs.next().unwrap(); // will be "anf"
            let anf = parse_anf(src, c, a);
            Anf::Neg(Box::new(anf))
        }
        3 => {
            // binary operation
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let l = cs.next().unwrap();
            let l = parse_anf(src, c, l);

            let op = cs.next().unwrap();
            let utf8 = op.utf8_text(src).unwrap();
            let op = String::from_utf8(utf8.into()).unwrap();
            assert!(
                op == "&&".to_string() || op == "||".to_string(),
                "invalid program found!\nsexp: {}",
                n.to_sexp()
            );

            let r = cs.next().unwrap();
            let r = parse_anf(src, c, r);

            if op == "&&".to_string() {
                Anf::And(Box::new(l), Box::new(r))
            } else {
                Anf::Or(Box::new(l), Box::new(r))
            }
        }
        _ => panic!("invalid program found!\nsexp: {}", n.to_sexp()),
    }
}
fn parse_anf_node(src: &[u8], c: &mut TreeCursor, n: Node) -> AnfInferable {
    let k = n.kind();
    match k {
        "identifier" => {
            let utf8 = n.utf8_text(src).unwrap();
            let var = String::from_utf8(utf8.into()).unwrap();
            Anf::AVar(None, var)
        }
        "bool" => {
            let utf8 = n.utf8_text(src).unwrap();
            let var = String::from_utf8(utf8.into()).unwrap();
            let b = if var == "true".to_string() {
                true
            } else if var == "false".to_string() {
                false
            } else {
                panic!("impossible")
            };
            Anf::AVal((), Val::Bool(b))
        }
        _ => panic!("invalid program found!\nsexp: {}", n.to_sexp()),
    }
}

fn parse_float_h(src: &[u8], c: &mut TreeCursor, n: Node) -> f64 {
    let utf8 = n.utf8_text(src).unwrap();
    let fstr = String::from_utf8(utf8.into()).unwrap();
    fstr.parse::<f64>().unwrap()
}
fn parse_float(src: &[u8], c: &mut TreeCursor, n: Node) -> f64 {
    match n.named_child_count() {
        0 => parse_float_h(src, c, n),
        1 => {
            let mut c_ = c.clone();
            let mut cs = n.named_children(&mut c_);
            let fnode = cs.next().unwrap();
            parse_float_h(src, c, fnode)
        }
        2 => panic!("impossible"),
        3 => {
            // division operation
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let l = cs.next().unwrap();
            let l = parse_float(src, c, l);

            let op = cs.next().unwrap();
            let utf8 = op.utf8_text(src).unwrap();
            let op = String::from_utf8(utf8.into()).unwrap();
            assert!(
                op == "/".to_string(),
                "invalid program found!\nsexp: {}",
                n.to_sexp()
            );

            let r = cs.next().unwrap();
            let r = parse_float(src, c, r);

            l / r
        }
        _ => panic!("unexpected"),
    }
}
fn parse_expr(src: &[u8], c: &mut TreeCursor, n: &Node) -> ExprInferable {
    let k = n.kind();
    match k {
        "anf" => {
            println!("{}", n.to_sexp());
            let anf = parse_anf(src, c, *n);
            return Expr::EAnf((), Box::new(anf));
        }
        // finished with Anf expressions
        "fst" => {
            // assert_children!(k, 1, n, c);
            // let mut _c = c.clone();
            // let mut cs = n.named_children(&mut _c);
            // let a = cs.next().unwrap();
            // let anf = parse_anf(src, c, a);
            // return Expr::EAnf((), Box::new(anf));
            todo!();
        }
        "snd" => {
            // assert!(n.named_child_count() == 1, "{k} #named_children: {}, ", n.named_child_count());
            // let mut _c = c.clone();
            // let mut cs = n.named_children(&mut _c);
            // let a = cs.next().unwrap();
            // let anf = parse_anf(src, c, a);
            // Expr::EAnf((), Box::new(anf))
            todo!();
        }
        // EPrj(<EPrjExt as ξ<X>>::Ext, usize, Box<Anf<X>>),
        "prod" => {
            assert!(n.named_child_count() == 2, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);
            let l = cs.next().unwrap();
            let l = parse_anf(src, c, l);

            let r = cs.next().unwrap();
            let r = parse_anf(src, c, r);

            assert!(cs.next().is_none(), "{k}");
            // Expr::EProd(
            //     Box::new(l),
            //     Box::new(r),
            //     Box::new(Ty::Prod(Box::new(tyl), Box::new(tyr))),
            // )
            todo!()
        }
        "let_binding" => {
            assert_children!(k, 3, n, c);
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let ident = cs.next().unwrap();
            let utf8 = ident.utf8_text(src).unwrap();
            let ident = String::from_utf8(utf8.into()).unwrap();

            let bindee = cs.next().unwrap();
            let bindee = parse_expr(src, c, &bindee);

            let body = cs.next().unwrap();
            let body = parse_expr(src, c, &body);
            Expr::ELetIn(None, ident, Box::new(bindee), Box::new(body))
        }
        "ite_binding" => {
            assert!(n.named_child_count() == 4, "{k} #named_children: {}\nsexp: {}", n.named_child_count(), n.to_sexp());
            todo!()
        }
        "flip" => {
            println!("{}", n.to_sexp());
            let f = parse_float(src, c, *n);
            Expr::EFlip((), f)
        }
        "observe" => {
            let anf = parse_anf(src, c, *n);
            Expr::EObserve((), Box::new(anf))
        }
        "sample" => {
            println!("{}", n.to_sexp());
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);
            let subp = cs.next().unwrap();
            let e = parse_expr(src, c, &subp);
            Expr::ESample((), Box::new(e))
        }
        s => panic!(
            "unexpected tree-sitter node kind `{}` (#named_children: {})! Likely, you need to rebuild the tree-sitter parser\nsexp: {}", s, n.named_child_count(), n.to_sexp()
        ),
    }
}

fn parse_program(src: &[u8], c: &mut TreeCursor, n: &Node) -> ProgramInferable {
    Program::Body(parse_expr(src, c, n))
}
pub fn parse_tree(src: &[u8], t: Tree) -> ProgramInferable {
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

pub fn parse(code: &str) -> Option<ProgramInferable> {
    let tree = tree_parser(code.to_string())?;
    let expr = parse_tree(code.as_bytes(), tree);
    Some(expr)
}

#[cfg(test)]
mod parser_tests {
    use super::*;
    use crate::*;
    use std::any::TypeId;

    #[test]
    fn parse_anf() {
        assert_eq!(parse(r#"true"#).unwrap(), program!(b!(true)));
        assert_eq!(parse(r#"false"#).unwrap(), program!(b!(false)));
        assert_eq!(parse(r#"x"#).unwrap(), program!(b!("x")));
        assert_eq!(parse(r#"!a"#).unwrap(), program!(anf!(not!("a"))));
        assert_eq!(parse(r#"a && b"#).unwrap(), program!(b!("a" && "b")));
        assert_eq!(parse(r#"a || b"#).unwrap(), program!(b!("a" || "b")));
        assert_eq!(
            parse(r#"a && b && c"#).unwrap(),
            program!(b!("a" && "b" && "c"))
        );
    }

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
    fn one_let() {
        let code = r#"let x = true in x"#;
        let expr = parse(code);
        assert_eq!(
            expr.unwrap(),
            program!(lets!["x" ;= b!("true"); in var!("x")])
        );
    }

    /// ======================
    /// exact: one untyped flip
    /// ======================
    ///
    /// let x = flip (1/3) in
    /// x
    /// ---
    ///
    /// (source_file
    ///   (let_binding
    ///     (identifier) (flip (float) (float_op) (float))
    ///     (anf (identifier))))
    #[test]
    fn one_untyped_flip() {
        assert_eq!(
            parse(r#"let x = flip 0.5 in x"#).unwrap(),
            program!(lets!["x" ;= flip!(0.5); in var!("x")])
        );
        assert_eq!(
            parse(r#"let x = flip (1/3) in x"#).unwrap(),
            program!(lets!["x" ;= flip!(1.0/3.0); in var!("x")])
        );
    }

    /// ===========================
    /// exact: observed two flips
    /// ===========================
    ///
    /// let x = flip 0.3333 in
    /// let y = flip 1/4 in
    /// let _ = observe (x || y) in
    /// x
    ///
    /// ---
    ///
    ///
    /// (source_file
    ///   (let_binding
    ///     (identifier)
    ///     (flip (float))
    ///   (let_binding
    ///     (identifier)
    ///     (flip (float) (float_op) (float))
    ///   (let_binding
    ///     (identifier)
    ///     (observe (anf (anf (identifier)) (bool_biop) (anf (identifier))))
    ///   (anf (identifier))))))
    #[test]
    fn observed_two_flips() {
        let code = r#"
        let x = flip 0.3333 in
        let y = flip 1/4 in
        let _ = observe (x || y) in
        x"#;
        let expr = parse(code);
        assert_eq!(
            expr.unwrap(),
            program!(lets![
                "x" ;= flip!(0.3333);
                "y" ;= flip!(1.0/4.0);
                "_" ;= observe!(b!("x" || "y"));
                ...? b!("x")])
        );
    }

    /// ======================
    /// approx: observed two flips
    /// ======================
    ///
    /// let x = sample (flip 1/3) in
    /// let y = flip 1/4 in
    /// let _ = observe (x || y) in
    /// x
    ///
    /// ---
    ///
    /// (source_file
    ///   (let_binding
    ///     (identifier)
    ///     (sample (flip (float) (float_op) (float)))
    ///   (let_binding
    ///     (identifier)
    ///     (flip (float) (float_op) (float))
    ///   (let_binding
    ///     (identifier)
    ///     (observe (anf (anf (identifier)) (bool_biop) (anf (identifier))))
    ///   (anf (identifier))))))
    #[test]
    fn observed_two_flips_with_sample() {
        let code = r#"
        let x = sample (flip 1/3) in
        let y = flip 1/4 in
        let _ = observe (x || y) in
        x"#;
        let expr = parse(code);
        assert_eq!(
            expr.unwrap(),
            program!(lets![
                "x" ;= sample!(flip!(1.0/3.0));
                "y" ;= flip!(1.0/4.0);
                "_" ;= observe!(b!("x" || "y"));
                ...? b!("x")])
        );
    }
}
