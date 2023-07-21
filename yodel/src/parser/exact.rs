use crate::grammar::*;
use crate::typeinf::grammar::{AnfInferable, EExprInferable, Inferable, ProgramInferable};
use itertools::Itertools;
use rsdd::builder::bdd_plan::BddPlan;
use std::collections::VecDeque;
use tree_sitter::*;

use super::shared::*;
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

fn parse_anf_child_h(src: &[u8], c: &mut TreeCursor, n: Node) -> Anf<Inferable, EVal> {
    let mut c_ = c.clone();
    let mut cs = n.named_children(&mut c_);
    let a = cs.next().unwrap();
    parse_anf(src, c, a)
}

fn parse_anf(src: &[u8], c: &mut TreeCursor, n: Node) -> Anf<Inferable, EVal> {
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

            let r = cs.next().unwrap();
            let r = parse_anf(src, c, r);
            match op.as_str() {
                "&&" => Anf::And(Box::new(l), Box::new(r)),
                "||" => Anf::Or(Box::new(l), Box::new(r)),
                "/" => Anf::Div(Box::new(l), Box::new(r)),
                "*" => Anf::Mult(Box::new(l), Box::new(r)),
                "+" => Anf::Plus(Box::new(l), Box::new(r)),
                "-" => Anf::Minus(Box::new(l), Box::new(r)),
                _ => panic!("invalid program found!\nsexp: {}", n.to_sexp()),
            }
        }
        _ => panic!("invalid program found!\nsexp: {}", n.to_sexp()),
    }
}

fn parse_eval(src: &[u8], c: &mut TreeCursor, n: Node) -> EVal {
    match n.kind() {
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
            EVal::EBdd(BddPlan::from_bool(b))
        }
        "int" => {
            let utf8 = n.utf8_text(src).unwrap();
            let istr = String::from_utf8(utf8.into()).unwrap();
            let ix = istr.parse::<usize>().unwrap();
            EVal::EInteger(ix)
        }
        "float" => {
            let utf8 = n.utf8_text(src).unwrap();
            let istr = String::from_utf8(utf8.into()).unwrap();
            let x = istr.parse::<f64>().unwrap();
            EVal::EFloat(x)
        }
        _ => panic!("invalid value! found sexp:\n{}", n.to_sexp()),
    }
}
fn parse_anf_enode(src: &[u8], c: &mut TreeCursor, n: Node) -> Anf<Inferable, EVal> {
    let k = n.kind();
    match k {
        "identifier" => {
            let utf8 = n.utf8_text(src).unwrap();
            let var = String::from_utf8(utf8.into()).unwrap();
            Anf::AVar(None, var)
        }
        _ => {
            if k == "bool" || k == "float" || k == "int" {
                let eval = parse_eval(src, c, n);
                Anf::AVal((), eval)
            } else {
                panic!("invalid anf! found sexp:\n{}", n.to_sexp())
            }
        }
    }
}

pub fn parse_etype(src: &[u8], c: &mut TreeCursor, n: &Node) -> ETy {
    use ETy::*;
    assert_eq!(n.kind(), "ety");
    let mut c_ = c.clone();
    let mut cs = n.named_children(&mut c_);
    let n = cs.next().unwrap();
    let k = n.kind();
    match k {
        "tyBool" => {
            ETy::EBool
        }
        "tyFloat" => {
            ETy::EFloat
        }
        "tyInt" => {
            ETy::EInt
        }
        "tyProd" => {
            ETy::EProd(parse_vec(src, c, n, |a, b, c| parse_etype(a, b, &c)))
        }
        s => panic!(
            "unexpected tree-sitter type (kind `{}`) (#named_children: {})! Likely, you need to rebuild the tree-sitter parser\nsexp: {}", s, n.named_child_count(), n.to_sexp()
        ),
    }
}

pub fn parse_eexpr(src: &[u8], c: &mut TreeCursor, n: &Node) -> EExpr<Inferable> {
    use EExpr::*;
    use SExpr::SExact;
    assert_eq!(n.kind(), "eexpr");
    let mut c_ = c.clone();
    let mut cs = n.named_children(&mut c_);
    let n = cs.next().unwrap();

    let k = n.kind();
    match k {
        "eexpr" => {
            // made it to a nested paren! run again
            parse_eexpr(src, c, &n)
        }
        "eanf" => {
            println!("{}", n.to_sexp());
            let anf = parse_anf(src, c, n);
            EExpr::EAnf((), Box::new(anf))
        }
        "efst" => {  // sugar for anf projection for now
            let mut c_ = c.clone();
            let mut cs = n.named_children(&mut c_);
            let anf = cs.next().unwrap();
            let anf = parse_anf(src, c, anf);
            EExpr::EAnf((), Box::new(Anf::AnfPrj(Box::new(anf), Box::new(Anf::AVal((), EVal::EInteger(0))))))
            // EExpr::EPrj(None, Box::new(Anf::AVal((), EVal::EInteger(0))), Box::new(anf))
        }
        "esnd" => {  // sugar for anf projection for now
            let mut c_ = c.clone();
            let mut cs = n.named_children(&mut c_);
            let anf = cs.next().unwrap();
            let anf = parse_anf(src, c, anf);
            EExpr::EAnf((), Box::new(Anf::AnfPrj(Box::new(anf), Box::new(Anf::AVal((), EVal::EInteger(1))))))
            // EExpr::EPrj(None, Box::new(Anf::AVal((), EVal::EInteger(1))), Box::new(anf))
        }
        "eprj" => {  // sugar for anf projection for now
            let mut c_ = c.clone();
            let mut cs = n.named_children(&mut c_);

            let anf = cs.next().unwrap();
            let anf = parse_anf(src, c, anf);

            let ix = cs.next().unwrap();
            let ix = parse_anf(src, c, ix);

            EExpr::EAnf((), Box::new(Anf::AnfPrj(Box::new(anf), Box::new(ix))))
            // EExpr::EPrj(None, Box::new(ix), Box::new(anf))
        }
        "eprod" => {  // sugar for anf projection for now
            let mut anfs = vec![];
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);
            for _ in 0..n.named_child_count() {
                let a = cs.next().unwrap();
                let a = parse_anf(src, c, a);
                anfs.push(a);
            }
            EExpr::EAnf((), Box::new(Anf::AnfProd(anfs)))
            // EExpr::EProd(None, anfs)
        }
        "eapp" => {
            assert_children!(k, 2, n, c);
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let fnname = cs.next().unwrap();
            let fnname = parse_str(src, &fnname);

            let args = parse_vec(src, c, n, |a, b, c| parse_anf(a, b, c));

            EExpr::EApp(None, fnname, args)
        }
        "ediscrete" => {
            let mut params = vec![];
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);
            for _ in 0..n.named_child_count() {
                let n = cs.next().unwrap();
                let f = parse_anf(src, &mut c.clone(), n);
                params.push(f);
            }
            EExpr::EDiscrete((), params)
        }
        "elet" => {
            println!("{}", n.to_sexp());
            assert_children!(k, 3, n, c);
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let ident = cs.next().unwrap();
            let utf8 = ident.utf8_text(src).unwrap();
            let ident = String::from_utf8(utf8.into()).unwrap();

            let bindee = cs.next().unwrap();
            let bindee = parse_eexpr(src, c, &bindee);

            let body = cs.next().unwrap();
            let body = parse_eexpr(src, c, &body);
            EExpr::ELetIn(None, ident, Box::new(bindee), Box::new(body))
        }
        "eite" => {
            println!("{}", n.to_sexp());
            assert_children!(k, 3, n, c);
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let pred = cs.next().unwrap();
            let pred = parse_anf(src, c, pred);

            let tbranch = cs.next().unwrap();
            let tbranch = parse_eexpr(src, c, &tbranch);

            let fbranch = cs.next().unwrap();
            let fbranch = parse_eexpr(src, c, &fbranch);
            EExpr::EIte(None, Box::new(pred), Box::new(tbranch), Box::new(fbranch))
        }
        "eflip" => {
            println!("{}", n.to_sexp());
            let f = parse_anf(src, c, n);
            EExpr::EFlip((), Box::new(f))
        }
        "eobserve" => {
            let anf = parse_anf(src, c, n);
            EExpr::EObserve((), Box::new(anf))
        }
        "esample" => {
            println!("{}", n.to_sexp());
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);
            let subp = cs.next().unwrap();
            let sexpr = super::sampling::parse_sexpr(src, c, &subp);
            EExpr::ESample((), Box::new(sexpr))
        }
        "eiterate" => {
            println!("{}", n.to_sexp());
            assert_children!(k, 3, n, c);
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let ident = cs.next().unwrap();
            let utf8 = ident.utf8_text(src).unwrap();
            let ident = String::from_utf8(utf8.into()).unwrap();

            let init = cs.next().unwrap();
            let init = parse_anf(src, c, init);

            let niters = cs.next().unwrap();
            let niters = parse_anf(src, c, niters);
            EExpr::EIterate((), ident, Box::new(init), Box::new(niters))
        }
        s => panic!(
            "unexpected tree-sitter node kind `{}` (#named_children: {})! Likely, you need to rebuild the tree-sitter parser\nsexp: {}", s, n.named_child_count(), n.to_sexp()
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::program::parse;
    use crate::*;
    use std::any::TypeId;

    #[test]
    fn parse_anf() {
        assert_eq!(parse(r#"exact {true}"#).unwrap(), program!(b!(true)));
        assert_eq!(parse(r#"exact {false}"#).unwrap(), program!(b!(false)));
        assert_eq!(parse(r#"exact {x}"#).unwrap(), program!(b!("x")));
        assert_eq!(parse(r#"exact {!a}"#).unwrap(), program!(anf!(not!("a"))));
        assert_eq!(
            parse(r#"exact {a && b}"#).unwrap(),
            program!(b!("a" && "b"))
        );
        assert_eq!(
            parse(r#"exact {a || b}"#).unwrap(),
            program!(b!("a" || "b"))
        );
        assert_eq!(
            parse(r#"exact { a && b && c }"#).unwrap(),
            program!(b!("a" && "b" && "c"))
        );
    }

    #[test]
    fn prods() {
        assert_eq!(
            parse(r#"exact { (a, b, c) }"#).unwrap(),
            program!(b!("a", "b", "c"))
        );
        assert_eq!(
            parse(r#"exact { let x = (a, b) in fst x }"#).unwrap(),
            program!(lets!["x" ;= b!("a", "b"); ...? fst!("x")])
        );
        assert_eq!(
            parse(r#"exact { let x = (a, b) in snd x }"#).unwrap(),
            program!(lets!["x" ;= b!("a", "b"); ...? snd!("x")])
        );
        let p = parse(r#"exact { let x = (a, b) in (prj 0 x) }"#).unwrap();
        println!("{:?}", p);
        assert_eq!(
            parse(r#"exact { let x = (a, b) in (prj 0 x) }"#).unwrap(),
            program!(lets!["x" ;= b!("a", "b"); ...? prj!(0, "x")])
        );
    }

    #[test]
    fn one_ite() {
        assert_eq!(
            parse(r#"exact {if true then x else y}"#).unwrap(),
            program!(ite!( if ( b!(true) ) then { b!("x")  } else { b!("y") } ))
        );
    }

    // ======================
    // exact: untyped trivial
    // ======================
    //
    // exact {
    //   let x = true in x
    // }
    //
    // ---
    //
    //     (source_file
    //       (program
    //         (eexpr
    //           (elet
    //             (identifier)
    //             (eexpr
    //               (eanf
    //                 (evalue
    //                   (bool))))
    //             (eexpr
    //               (eanf
    //                 (identifier)))))))
    #[test]
    fn one_let() {
        let code = r#"exact { let x = true in x }"#;
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
            parse(r#"exact { let x = flip 0.5 in x }"#).unwrap(),
            program!(lets!["x" ;= flip!(0.5); in var!("x")])
        );
        assert_eq!(
            parse(r#"exact { let x = flip (1/3) in x }"#).unwrap(),
            program!(lets!["x" ;= flip!(1/3); in var!("x")])
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
        let code = r#"exact {
          let x = flip 0.3333 in
          let y = flip 1/4 in
          let _ = observe (x || y) in
          x
        }"#;
        let expr = parse(code);
        assert_eq!(
            expr.unwrap(),
            program!(lets![
                "x" ;= flip!(0.3333);
                "y" ;= flip!(1/4);
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
        let code = r#"exact {
          let x = sample { bern(1/3) } in
          let y = flip 1/4 in
          let _ = observe (x || y) in
          x
        }"#;

        let expr = parse(code);
        assert_eq!(
            expr.unwrap(),
            program!(lets![
                "x" ;= sample!(~ bern!(1/3));
                "y" ;= flip!(1/4);
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
        exact { discrete(0., 0.2, 1.5) }
        "#;
        let expr = parse(code);
        assert_eq!(expr.unwrap(), program!(discrete![0.0, 0.2, 1.5]));
    }
}
