use crate::grammar::*;
use crate::typeinf::grammar::{AnfInferable, EExprInferable, ProgramInferable};
use itertools::Itertools;
use std::collections::VecDeque;
use tree_sitter::*;

use super::*;
use tree_sitter_yodel;

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

// fn parse_anf<'a, 'b>(c: &'a mut TreeCursor<'b>, n: &'b Node) -> (ANF, Ty) {
// anf: $ => choice(
//   $.identifier,
//   $._value,
//   prec.left(3, seq($.anf, $.bool_biop, $.anf)),
//   prec.left(5, seq($.bool_unop, $.anf)),
// ),

fn parse_anf_child_h(src: &[u8], c: &mut TreeCursor, n: Node) -> AnfInferable<EVal> {
    let mut c_ = c.clone();
    let mut cs = n.named_children(&mut c_);
    let a = cs.next().unwrap();
    parse_anf(src, c, a)
}

fn parse_anf(src: &[u8], c: &mut TreeCursor, n: Node) -> AnfInferable<EVal> {
    match n.named_child_count() {
        0 => parse_anf_enode(src, c, n),
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

fn parse_anf_enode(src: &[u8], c: &mut TreeCursor, n: Node) -> AnfInferable<EVal> {
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

fn parse_expr(src: &[u8], c: &mut TreeCursor, n: &Node) -> ESugar {
    use EExpr::*;
    use SExpr::SExact;
    let k = n.kind();
    match k {
        "anf" => {
            println!("{}", n.to_sexp());
            let anf = parse_anf(src, c, *n);
            ESugar::Prim(EAnf((), Box::new(anf)))
        }
        "fst" => {
            let mut c_ = c.clone();
            let mut cs = n.named_children(&mut c_);
            let anf = cs.next().unwrap();
            let anf = parse_anf(src, c, anf);
            ESugar::Prim(EPrj(None, 0, Box::new(anf)))
        }
        "snd" => {
            let mut c_ = c.clone();
            let mut cs = n.named_children(&mut c_);
            let anf = cs.next().unwrap();
            let anf = parse_anf(src, c, anf);
            ESugar::Prim(EPrj(None, 1, Box::new(anf)))
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
            ESugar::Prim(EPrj(None, ix, Box::new(anf)))
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
            ESugar::Prim(EProd(None, anfs))
        }
        "discrete" => {
            let mut params = vec![];
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);
            for _ in 0..n.named_child_count() {
                let n = cs.next().unwrap();
                let f = parse_float(src, &mut c.clone(), n);
                params.push(f);
            }
            ESugar::Discrete(params)
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
            ESugar::LetIn(ident, Box::new(bindee), Box::new(body))
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
            ESugar::Ite(Box::new(pred), Box::new(tbranch), Box::new(fbranch))
        }
        "flip" => {
            println!("{}", n.to_sexp());
            let f = parse_anf(src, c, *n);
            ESugar::Prim(EFlip((), Box::new(f)))
        }
        "observe" => {
            let anf = parse_anf(src, c, *n);
            ESugar::Prim(EObserve((), Box::new(anf)))
        }
        "sample" => {
            println!("{}", n.to_sexp());
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);
            let subp = cs.next().unwrap();
            let e = parse_expr(src, c, &subp);
            ESugar::Sample(Box::new(SSugar::Exact(Box::new(e))))
        }
        s => panic!(
            "unexpected tree-sitter node kind `{}` (#named_children: {})! Likely, you need to rebuild the tree-sitter parser\nsexp: {}", s, n.named_child_count(), n.to_sexp()
        ),
    }
}

pub fn parse_tree(src: &[u8], t: Tree) -> ESugar {
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
    parse_expr(src, &mut c, &root)
}

#[cfg(test)]
mod exact_parser_tests {
    use super::*;
    use crate::*;
    use std::any::TypeId;

    fn parse(code: &str) -> Option<ProgramSugar> {
        let tree = tree_parser(code.to_string())?;
        let expr = parse_tree(code.as_bytes(), tree);
        Some(expr)
    }

    #[test]
    fn parse_anf() {
        assert_eq!(parse(r#"true"#).unwrap().desugar(), program!(b!(true)));
        assert_eq!(parse(r#"false"#).unwrap().desugar(), program!(b!(false)));
        assert_eq!(parse(r#"x"#).unwrap().desugar(), program!(b!("x")));
        assert_eq!(parse(r#"!a"#).unwrap().desugar(), program!(anf!(not!("a"))));
        assert_eq!(
            parse(r#"a && b"#).unwrap().desugar(),
            program!(b!("a" && "b"))
        );
        assert_eq!(
            parse(r#"a || b"#).unwrap().desugar(),
            program!(b!("a" || "b"))
        );
        assert_eq!(
            parse(r#"a && b && c"#).unwrap().desugar(),
            program!(b!("a" && "b" && "c"))
        );
    }

    #[test]
    fn prods() {
        assert_eq!(
            parse(r#"(a, b, c)"#).unwrap().desugar(),
            program!(b!("a", "b", "c"))
        );
        assert_eq!(
            parse(r#"let x = (a, b) in fst x"#).unwrap().desugar(),
            program!(lets!["x" ;= b!("a", "b"); ...? fst!("x")])
        );
        assert_eq!(
            parse(r#"let x = (a, b) in snd x"#).unwrap().desugar(),
            program!(lets!["x" ;= b!("a", "b"); ...? snd!("x")])
        );
        assert_eq!(
            parse(r#"let x = (a, b) in (prj 0 x)"#).unwrap().desugar(),
            program!(lets!["x" ;= b!("a", "b"); ...? prj!(0, "x")])
        );
    }

    #[test]
    fn one_ite() {
        assert_eq!(
            parse(r#"if true then x else y"#).unwrap().desugar(),
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
            expr.unwrap().desugar(),
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
            parse(r#"let x = flip 0.5 in x"#).unwrap().desugar(),
            program!(lets!["x" ;= flip!(0.5); in var!("x")])
        );
        assert_eq!(
            parse(r#"let x = flip (1/3) in x"#).unwrap().desugar(),
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
            expr.unwrap().desugar(),
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
            expr.unwrap().desugar(),
            program!(lets![
                "x" ;= sample!(flip!(1.0/3.0));
                "y" ;= flip!(1.0/4.0);
                "_" ;= observe!(b!("x" || "y"));
                ...? b!("x")])
        );
    }

    /// ======================
    /// discrete 3 arg
    /// ======================
    ///
    /// discrete(0, 0.2, 1.5)
    /// ---
    ///
    /// (source_file
    ///   (discrete
    ///     (float)
    ///     (float)
    ///     (float)))
    ///
    #[test]
    fn discrete_3_arg() {
        let code = r#"
        discrete(0, 0.2, 1.5)
        "#;
        let expr = parse(code);
        assert_eq!(expr.unwrap().desugar(), program!(discrete![0.0, 0.2, 1.5]));
    }
}
