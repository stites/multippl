use crate::grammar::*;
use crate::typeinf::grammar::{AnfInferable, EExprInferable, Inferable, ProgramInferable};
use itertools::Itertools;
use rsdd::builder::bdd_plan::BddPlan;
use std::collections::VecDeque;
use tree_sitter::*;

use super::shared::*;
use super::*;
use crate::*;
use tracing::*;
use tree_sitter_multippl;

macro_rules! assert_children {
    ( $src:expr, $x:expr, $count:literal, $node:expr, $c:expr ) => {{
        let mut c__ = $c.clone();
        let cs = $node.named_children(&mut c__).into_iter().collect_vec();
        assert!(
            $node.named_child_count() == $count,
            "{} #named_children: {} (expected {})\nchildren: {:?}\nsexp: {}\nsrc: {}\n",
            $x,
            $node.named_child_count(),
            $count,
            cs,
            $node.to_sexp(),
            parse_str($src, &$node)
        );
    }};
}

fn parse_eanf(src: &[u8], c: &mut TreeCursor, n: Node) -> Anf<Inferable, EVal> {
    assert_eq!(n.kind(), "eanf");
    let mut c_ = c.clone();
    let mut cs = n.named_children(&mut c_);
    let n = cs.next().unwrap();

    match n.kind() {
        "eanf" => parse_eanf(src, c, n),
        "eanfprod" => Anf::AnfProd(parse_vec(src, c, n, parse_eanf)),
        "eanfprj" => {
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let ident = cs.next().unwrap();
            let ident = Anf::AVar(None, parse_str(src, &ident));

            let prj = cs.next().unwrap();
            let prj = parse_eanf(src, c, prj);
            Anf::AnfPrj(Box::new(ident), Box::new(prj))
        }
        "evalue" => Anf::AVal((), parse_eval(src, c, n)),
        "identifier" => Anf::AVar(None, parse_str(src, &n)),
        "eanfunop" => {
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);
            let n = cs.next().unwrap();
            tracing::debug!(
                "parsing eanf unop: {} >>> {}",
                n.to_sexp(),
                parse_str(src, &n)
            );
            match n.kind() {
                "bool_unop" => {
                    let node = cs.next().unwrap(); // drop the operator symbol
                    Anf::Neg(Box::new(parse_eanf(src, c, node)))
                }
                _ => panic!("invalid unary operator found!\nsexp: {}", n.to_sexp()),
            }
        }
        "etrace" => {
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let tr = cs.next().unwrap();
            let tr = parse_eanf(src, c, tr);

            let x = cs.next().unwrap();
            let x = parse_eanf(src, c, x);
            Anf::AnfTrace(Box::new(tr), Box::new(x))
        }
        "eanfbinop" => {
            tracing::debug!(
                "parsing eanf binop: {} >>> {}",
                n.to_sexp(),
                parse_str(src, &n)
            );

            // binary operations
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let l = cs.next().unwrap();
            tracing::debug!("parsing anf 3, first node kind: {}", l.kind());
            let l = parse_eanf(src, c, l);

            let op = cs.next().unwrap();
            let op = parse_str(src, &op);

            let r = cs.next().unwrap();
            let r = parse_eanf(src, c, r);

            match op.as_str() {
                "&&" => Anf::And(Box::new(l), Box::new(r)),
                "||" => Anf::Or(Box::new(l), Box::new(r)),
                "/" => Anf::Div(Box::new(l), Box::new(r)),
                "*" => Anf::Mult(Box::new(l), Box::new(r)),
                "+" => Anf::Plus(Box::new(l), Box::new(r)),
                "-" => Anf::Minus(Box::new(l), Box::new(r)),
                "==" => Anf::EQ(Box::new(l), Box::new(r)),
                ">" => Anf::GT(Box::new(l), Box::new(r)),
                "<" => Anf::LT(Box::new(l), Box::new(r)),
                ">=" => Anf::GTE(Box::new(l), Box::new(r)),
                "<=" => Anf::LTE(Box::new(l), Box::new(r)),
                op => panic!("unknown binary operator found: {l:?} <{op}> {r:?}"),
            }
        }

        kind => {
            panic!(
                "invalid expression found with kind {} and {} children!\nsexp: {}\nsrc: {}",
                kind,
                n.named_child_count(),
                n.to_sexp(),
                parse_str(src, &n)
            )
        }
    }
}

fn parse_eval(src: &[u8], c: &mut TreeCursor, n: Node) -> EVal {
    assert_eq!(n.kind(), "evalue");
    let mut c_ = c.clone();
    let mut cs = n.named_children(&mut c_);
    let n = cs.next().unwrap();

    match n.kind() {
        "evalue" => parse_eval(src, c, n),
        "evalueprod" => {
            let mut vs = vec![];
            for n in cs {
                vs.push(parse_eval(src, c, n));
            }
            EVal::EProd(vs)
        }
        "bool" => {
            let var = parse_str(src, &n);
            let b = if var == *"true" {
                true
            } else if var == *"false" {
                false
            } else {
                panic!("impossible")
            };
            EVal::EBdd(BddPtr::from_bool(b))
        }
        "int" => {
            let istr = parse_str(src, &n);
            let ix = istr.parse::<usize>().unwrap();
            EVal::EInteger(ix)
        }
        "float" => {
            let istr = parse_str(src, &n);
            let x = istr.parse::<f64>().unwrap();
            EVal::EFloat(x)
        }
        _ => panic!("invalid value! found sexp:\n{}", n.to_sexp()),
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
        "etyProd" => {
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
        "comment" => {
            let n = cs.next().unwrap();
            // println!("{}>>> {}", n.kind(), n.to_sexp());
            parse_eexpr(src, c, &n)
        }
        "eexpr" => {
            // made it to a nested paren! run again
            parse_eexpr(src, c, &n)
        }
        "eanf" => {
            trace!("eanf: {} ||| {}", n.to_sexp(), parse_str(src, &n));
            let anf = parse_eanf(src, c, n);
            EExpr::EAnf((), Box::new(anf))
        }
        "efst" => {  // sugar for anf projection for now
            let mut c_ = c.clone();
            let mut cs = n.named_children(&mut c_);
            let anf = cs.next().unwrap();
            let anf = parse_eanf(src, c, anf);
            EExpr::EAnf((), Box::new(Anf::AnfPrj(Box::new(anf), Box::new(Anf::AVal((), EVal::EInteger(0))))))
            // EExpr::EPrj(None, Box::new(Anf::AVal((), EVal::EInteger(0))), Box::new(anf))
        }
        "esnd" => {  // sugar for anf projection for now
            let mut c_ = c.clone();
            let mut cs = n.named_children(&mut c_);
            let anf = cs.next().unwrap();
            let anf = parse_eanf(src, c, anf);
            EExpr::EAnf((), Box::new(Anf::AnfPrj(Box::new(anf), Box::new(Anf::AVal((), EVal::EInteger(1))))))
            // EExpr::EPrj(None, Box::new(Anf::AVal((), EVal::EInteger(1))), Box::new(anf))
        }
        "eapp" => {
            trace!("eapp: {}", parse_str(src, &n));
            trace!("sexp: {}", n.to_sexp());
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let fnname = cs.next().unwrap();
            let fnname = parse_str(src, &fnname);

            let args = parse_vec_h(src, c, n, parse_eanf, 1);

            EExpr::EApp(None, fnname, args)
        }
        "ediscrete" => {
            trace!("ediscrete: {}", parse_str(src, &n));
            let mut params = vec![];

            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);
            for _ in 0..n.named_child_count() {
                let n = cs.next().unwrap();
                let f = parse_eanf(src, &mut c.clone(), n);
                params.push(f);
            }
            EExpr::EDiscrete((), params)
        }
        "elet" => {
            trace!("elet");
            //println!("{}", n.to_sexp());
            assert_children!(src, k, 3, n, c);
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let ident = cs.next().unwrap();
            let ident = parse_str(src, &ident);
            trace!("elet, ident: {ident}");

            let bindee = cs.next().unwrap();
            trace!("elet, bindee: {}", parse_str(src, &bindee) );
            let bindee = parse_eexpr(src, c, &bindee);
            trace!("elet, bindee: {bindee:?}");

            let body = cs.next().unwrap();
            let body = parse_eexpr(src, c, &body);
            trace!("elet, body: {body:?}");
            EExpr::ELetIn(None, ident, Box::new(bindee), Box::new(body))
        }
        "eite" => {
            //println!("{}", n.to_sexp());
            assert_children!(src, k, 3, n, c);
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let pred = cs.next().unwrap();
            let pred = parse_eanf(src, c, pred);

            let tbranch = cs.next().unwrap();
            let tbranch = parse_eexpr(src, c, &tbranch);

            let fbranch = cs.next().unwrap();
            let fbranch = parse_eexpr(src, c, &fbranch);
            EExpr::EIte(None, Box::new(pred), Box::new(tbranch), Box::new(fbranch))
        }
        "eflip" => {
            trace!("eflip");
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let param = cs.next().unwrap();
            let param = parse_eanf(src, c, param);

            EExpr::EFlip((), Box::new(param))
        }
        "eobserve" => {
            trace!("eobserve");
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let anf = cs.next().unwrap();
            let anf = parse_eanf(src, c, anf);

            let body = cs.next().unwrap();
            let body = parse_eexpr(src, c, &body);

            EExpr::EObserve((), Box::new(anf), Box::new(body))
        }
        "esample" => {
            //println!("{}", n.to_sexp());
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let sexpr = cs.next().unwrap();
            let sexpr = super::sampling::parse_sexpr(src, c, &sexpr);

            EExpr::ESample((), Box::new(sexpr))
        }
        "eiterate" => {
            //println!("{}", n.to_sexp());
            assert_children!(src, k, 3, n, c);
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let ident = cs.next().unwrap();
            let utf8 = ident.utf8_text(src).unwrap();
            let ident = String::from_utf8(utf8.into()).unwrap();

            let init = cs.next().unwrap();
            let init = parse_eanf(src, c, init);

            let niters = cs.next().unwrap();
            let niters = parse_eanf(src, c, niters);
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
    use tracing_test::*;

    #[test]
    fn parse_eanf() {
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
    #[traced_test]
    fn prods() {
        println!("prods0");
        assert_eq!(
            parse(r#"exact { (a, b, c) }"#).unwrap(),
            program!(b!("a", "b", "c"))
        );
        println!("prods1");
        let p = parse(r#"exact { let x = (a, b) in fst x }"#).unwrap();
        assert_eq!(p, program!(lets!["x" ;= b!("a", "b"); ...? fst!("x")]));
        println!("prods2");
        let p = parse(r#"exact { let x = (a, b) in snd x }"#).unwrap();
        println!("{:?}", p);
        assert_eq!(p, program!(lets!["x" ;= b!("a", "b"); ...? snd!("x")]));
        println!("prods3");
        let p = parse(r#"exact { let x = (a, b) in x[0] }"#).unwrap();
        println!("{:?}", p);
        assert_eq!(
            parse(r#"exact { let x = (a, b) in x[0] }"#).unwrap(),
            program!(lets!["x" ;= b!("a", "b"); ...? prj!("x", 0)])
        );
        println!("prods4");
        let f = Anf::AVal((), EVal::EBdd(BddPtr::PtrFalse));
        let t = Anf::AVal((), EVal::EBdd(BddPtr::PtrTrue));
        let prd = EExpr::EAnf((), Box::new(Anf::AnfProd(vec![f, t])));
        assert_eq!(
            parse(r#"exact { let x = (false, true) in fst x }"#).unwrap(),
            program!(lets!["x" ;= prd; ...? prj!("x", 0)])
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
    #[traced_test]
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
    /// observe (x || y) in
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
          observe (x || y) in
          x
        }"#;
        let expr = parse(code);
        assert_eq!(
            expr.unwrap(),
            program!(lets![
                "x" ;= flip!(0.3333);
                "y" ;= flip!(1/4);
                ...? observe!(b!("x" || "y")
                => b!("x"))])
        );
    }

    /// ======================
    /// approx: observed two flips
    /// ======================
    ///
    /// let x = sample (flip 1/3) in
    /// let y = flip 1/4 in
    /// observe (x || y) in
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
          observe (x || y) in
          x
        }"#;

        let expr = parse(code);
        assert_eq!(
            expr.unwrap(),
            program!(lets![
                "x" ;= sample!(~ bern!(1/3));
                "y" ;= flip!(1/4);
                ...? observe!(b!("x" || "y")
                => b!("x"))])
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

    #[test]
    #[traced_test]
    fn function_call() {
        let code = r#"
        exact { apply(y, 0, y, 0, y) }
        "#;
        let expr = parse(code);
        let zero = Anf::AVal((), EVal::EInteger(0));
        let y = Anf::AVar(None, "y".to_string());
        let call = EExpr::EApp(
            None,
            "apply".to_string(),
            vec![y.clone(), zero.clone(), y.clone(), zero, y],
        );

        assert_eq!(expr.unwrap(), program!(call));
    }
}
