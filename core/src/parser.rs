use crate::typeinf::grammar::{AnfInferable, EExprInferable, ProgramInferable};
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
            Anf::AVal((), EVal::EBool(b))
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
fn parse_expr(src: &[u8], c: &mut TreeCursor, n: &Node) -> EExprInferable {
    let k = n.kind();
    match k {
        "anf" => {
            println!("{}", n.to_sexp());
            let anf = parse_anf(src, c, *n);
            return EExpr::EAnf((), Box::new(anf));
        }
        "fst" => {
            let mut c_ = c.clone();
            let mut cs = n.named_children(&mut c_);
            let anf = cs.next().unwrap();
            let anf = parse_anf(src, c, anf);
            EExpr::EPrj(None, 0, Box::new(anf))
        }
        "snd" => {
            let mut c_ = c.clone();
            let mut cs = n.named_children(&mut c_);
            let anf = cs.next().unwrap();
            let anf = parse_anf(src, c, anf);
            EExpr::EPrj(None, 1, Box::new(anf))
        }
        "prj" => {
            let mut c_ = c.clone();
            let mut cs = n.named_children(&mut c_);

            let i = cs.next().unwrap();
            let utf8 = i.utf8_text(src).unwrap();
            let istr = String::from_utf8(utf8.into()).unwrap();
            let ix = istr.parse::<usize>().unwrap();

            let anf = cs.next().unwrap();
            let anf = parse_anf(src, c, anf);
            EExpr::EPrj(None, ix, Box::new(anf))
        }
        "prod" => {
            let mut anfs = vec![];
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);
            for _ in 0..n.named_child_count() {
                let a = cs.next().unwrap();
                let a = parse_anf(src, c, a);
                anfs.push(a);
            }
            EExpr::EProd(None, anfs)
        }
        "let_binding" => {
            println!("{}", n.to_sexp());
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
            EExpr::ELetIn(None, ident, Box::new(bindee), Box::new(body))
        }
        "ite_binding" => {
            println!("{}", n.to_sexp());
            assert_children!(k, 3, n, c);
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let pred = cs.next().unwrap();
            let pred = parse_anf(src, c, pred);

            let tbranch = cs.next().unwrap();
            let tbranch = parse_expr(src, c, &tbranch);

            let fbranch = cs.next().unwrap();
            let fbranch = parse_expr(src, c, &fbranch);
            EExpr::EIte(None, Box::new(pred), Box::new(tbranch), Box::new(fbranch))
        }
        "flip" => {
            println!("{}", n.to_sexp());
            let f = parse_float(src, c, *n);
            EExpr::EFlip((), f)
        }
        "observe" => {
            let anf = parse_anf(src, c, *n);
            EExpr::EObserve((), Box::new(anf))
        }
        "sample" => {
            println!("{}", n.to_sexp());
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);
            let subp = cs.next().unwrap();
            let e = parse_expr(src, c, &subp);
            EExpr::ESample((), Box::new(e))
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

    #[test]
    fn prods() {
        assert_eq!(parse(r#"(a, b, c)"#).unwrap(), program!(b!("a", "b", "c")));
        assert_eq!(
            parse(r#"let x = (a, b) in fst x"#).unwrap(),
            program!(lets!["x" ;= b!("a", "b"); ...? fst!("x")])
        );
        assert_eq!(
            parse(r#"let x = (a, b) in snd x"#).unwrap(),
            program!(lets!["x" ;= b!("a", "b"); ...? snd!("x")])
        );
        assert_eq!(
            parse(r#"let x = (a, b) in (prj 0 x)"#).unwrap(),
            program!(lets!["x" ;= b!("a", "b"); ...? prj!(0, "x")])
        );
    }

    #[test]
    fn one_ite() {
        assert_eq!(
            parse(r#"if true then x else y"#).unwrap(),
            program!(ite!( if ( b!(true) ) then { b!("x")  } else { b!("y") } ))
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
