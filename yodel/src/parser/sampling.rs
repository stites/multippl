use super::exact::parse_eexpr;
use super::shared::*;
use crate::grammar::*;
use crate::typeinf::grammar::{AnfInferable, Inferable, ProgramInferable, SExprInferable};
use itertools::Itertools;
use std::collections::VecDeque;
use tree_sitter::*;

use super::*;
use tree_sitter_yodel;

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

fn parse_sanf(src: &[u8], c: &mut TreeCursor, n: Node) -> Anf<Inferable, SVal> {
    assert_eq!(n.kind(), "sanf");
    let mut c_ = c.clone();
    let mut cs = n.named_children(&mut c_);
    let n = cs.next().unwrap();

    match n.kind() {
        "sanf" => parse_sanf(src, c, n),
        "sanfprod" => Anf::AnfProd(parse_vec(src, c, n, parse_sanf)),
        "sanfvec" => Anf::AnfVec(parse_vec(src, c, n, parse_sanf)),
        "sanfprj" => {
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let ident = cs.next().unwrap();
            let ident = Anf::AVar(None, parse_str(src, &ident));

            let prj = cs.next().unwrap();
            let prj = parse_sanf(src, c, prj);
            Anf::AnfPrj(Box::new(ident), Box::new(prj))
        }
        "svalue" => Anf::AVal((), parse_sval(src, c, &n)),
        "identifier" => Anf::AVar(None, parse_str(src, &n)),
        "sanfbern" => {
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);
            let node = cs.next().unwrap();

            Anf::AnfBernoulli((), Box::new(parse_sanf(src, c, node)))
        }
        "sanfpoisson" => {
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);
            let node = cs.next().unwrap();

            Anf::AnfPoisson((), Box::new(parse_sanf(src, c, node)))
        }
        "sanfdirichlet" => Anf::AnfDirichlet((), parse_vec(src, c, n, parse_sanf)),
        "sanfdiscrete" => Anf::AnfDiscrete((), parse_vec(src, c, n, parse_sanf)),
        "sanfunop" => {
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);
            let n = cs.next().unwrap();
            tracing::debug!(
                "parsing sanf unop: {} >>> {}",
                n.to_sexp(),
                parse_str(src, &n)
            );
            match n.kind() {
                "bool_unop" => {
                    let node = cs.next().unwrap(); // drop the operator symbol
                    Anf::Neg(Box::new(parse_sanf(src, c, node)))
                }
                _ => panic!("invalid unary operator found!\nsexp: {}", n.to_sexp()),
            }
        }
        "sanfbinop" => {
            tracing::debug!(
                "parsing sanf binop: {} >>> {}",
                n.to_sexp(),
                parse_str(src, &n)
            );

            // binary operations
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let l = cs.next().unwrap();
            tracing::debug!("parsing anf 3, first node kind: {}", l.kind());
            let l = parse_sanf(src, c, l);

            let op = cs.next().unwrap();
            let op = parse_str(src, &op);

            let r = cs.next().unwrap();
            let r = parse_sanf(src, c, r);

            match op.as_str() {
                "&&" => Anf::And(Box::new(l), Box::new(r)),
                "||" => Anf::Or(Box::new(l), Box::new(r)),

                "/" => Anf::Div(Box::new(l), Box::new(r)),
                "*" => Anf::Mult(Box::new(l), Box::new(r)),
                "+" => Anf::Plus(Box::new(l), Box::new(r)),
                "-" => Anf::Minus(Box::new(l), Box::new(r)),
                "<" => Anf::LT(Box::new(l), Box::new(r)),
                "<=" => Anf::LTE(Box::new(l), Box::new(r)),
                ">" => Anf::GT(Box::new(l), Box::new(r)),
                ">=" => Anf::GTE(Box::new(l), Box::new(r)),
                "==" => Anf::EQ(Box::new(l), Box::new(r)),

                op => panic!("unknown binary operator found: {l:?} <{op}> {r:?}"),
            }
        }
        "sanfuniform" => {
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let c1 = cs.next().unwrap();
            let c1 = parse_sanf(src, c, c1);

            let c2 = cs.next().unwrap();
            let c2 = parse_sanf(src, c, c2);

            Anf::AnfUniform((), Box::new(c1), Box::new(c2))
        }
        "sanfnormal" => {
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let c1 = cs.next().unwrap();
            let c1 = parse_sanf(src, c, c1);

            let c2 = cs.next().unwrap();
            let c2 = parse_sanf(src, c, c2);

            Anf::AnfNormal((), Box::new(c1), Box::new(c2))
        }
        "sanfbeta" => {
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let c1 = cs.next().unwrap();
            let c1 = parse_sanf(src, c, c1);

            let c2 = cs.next().unwrap();
            let c2 = parse_sanf(src, c, c2);

            Anf::AnfBeta((), Box::new(c1), Box::new(c2))
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

pub fn parse_sval(src: &[u8], c: &mut TreeCursor, n: &Node) -> SVal {
    assert_eq!(n.kind(), "svalue");
    let mut c_ = c.clone();
    let mut cs = n.named_children(&mut c_);
    let n = cs.next().unwrap();

    use Dist::*;
    match shared::parse_gval(src, c, &n) {
        Some(GVal::Bool(x)) => SVal::SBool(x),
        Some(GVal::Float(x)) => SVal::SFloat(x),
        Some(GVal::Int(x)) => SVal::SInt(x),
        None => match n.kind() {
            "svalue" => parse_sval(src, c, &n),
            "svec" => SVal::SVec(parse_vec(src, c, n, |a, b, c| parse_sval(a, b, &c))),
            "sbern" => {
                let mut _c = c.clone();
                let mut cs = n.named_children(&mut _c);

                let p = cs.next().unwrap();
                let p = parse_num(src, &p);

                SVal::SDist(Bern(p))
            }
            "spoisson" => {
                let mut _c = c.clone();
                let mut cs = n.named_children(&mut _c);

                let p = cs.next().unwrap();
                let p = parse_num(src, &p);

                SVal::SDist(Poisson(p))
            }
            "suniform" => {
                let mut _c = c.clone();
                let mut cs = n.named_children(&mut _c);

                let a0 = cs.next().unwrap();
                let a0 = parse_num(src, &a0);

                let a1 = cs.next().unwrap();
                let a1 = parse_num(src, &a1);

                SVal::SDist(Uniform(a0, a1))
            }
            "snormal" => {
                let mut _c = c.clone();
                let mut cs = n.named_children(&mut _c);

                let a0 = cs.next().unwrap();
                let a0 = parse_num(src, &a0);

                let a1 = cs.next().unwrap();
                let a1 = parse_num(src, &a1);

                SVal::SDist(Normal(a0, a1))
            }
            "sbeta" => {
                let mut _c = c.clone();
                let mut cs = n.named_children(&mut _c);

                let a0 = cs.next().unwrap();
                let a0 = parse_num(src, &a0);

                let a1 = cs.next().unwrap();
                let a1 = parse_num(src, &a1);

                SVal::SDist(Beta(a0, a1))
            }
            "sdiscrete" => SVal::SDist(Discrete(parse_vec(src, c, n, |a, b, c| parse_num(a, &c)))),
            "sdirichlet" => {
                SVal::SDist(Dirichlet(parse_vec(src, c, n, |a, b, c| parse_num(a, &c))))
            }
            _ => panic!("invalid value! found sexp:\n{}", n.to_sexp()),
        },
    }
}

pub fn parse_stype(src: &[u8], c: &mut TreeCursor, n: &Node) -> STy {
    assert_eq!(n.kind(), "sty");
    let mut c_ = c.clone();
    let mut cs = n.named_children(&mut c_);
    let n = cs.next().unwrap();
    match n.kind() {
        "tyBool" => {
            STy::SBool
        }
        "tyFloat" => {
            STy::SFloat
        }
        "tyInt" => {
            STy::SInt
        }
        "tyDistribution" => {
            STy::SDistribution
        }
        "tyVec" => {
            let mut c_ = c.clone();
            let mut cs = n.named_children(&mut c_);

            let ty = cs.next().unwrap();
            let ty = parse_stype(src, c, &ty);

            STy::SVec(Box::new(ty))
        }
        "styProd" => {
            STy::SProd(parse_vec(src, c, n, |a, b, c| parse_stype(a, b, &c)))
        }
        s => panic!(
            "unexpected tree-sitter type (kind `{}`) (#named_children: {})! Likely, you need to rebuild the tree-sitter parser\nsexp: {}", s, n.named_child_count(), n.to_sexp()
        ),
    }
}

pub fn parse_sexpr(src: &[u8], c: &mut TreeCursor, n: &Node) -> SExpr<Inferable> {
    use SExpr::*;
    assert_eq!(n.kind(), "sexpr");
    let mut c_ = c.clone();
    let mut cs = n.named_children(&mut c_);
    let n = cs.next().unwrap();
    // println!("parse sexpr: {}", n.to_sexp());
    let k = n.kind();
    match k {
        "sexpr" => {
            // made it to a nested paren! run again
            parse_sexpr(src, c, &n)
        }
        "comment" => {
            // println!("parsing sanf: {} >>> {}", n.to_sexp(), parse_str(src, &n));
            let n = cs.next().unwrap();
            parse_sexpr(src, c, &n)
        }
        "sanf" => {
            tracing::debug!("parsing sanf: {} >>> {}", n.to_sexp(), parse_str(src, &n));
            let anf = parse_sanf(src, c, n);
            SExpr::SAnf((), Box::new(anf))
        }
        "slet" => {
            tracing::debug!("parsing slet: {} >>> {}", n.to_sexp(), parse_str(src, &n));
            assert_children!(src, k, 3, n, c);
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let ident = cs.next().unwrap();
            let ident = parse_str(src, &ident);

            let bindee = cs.next().unwrap();
            let bindee = parse_sexpr(src, c, &bindee);

            let body = cs.next().unwrap();
            let body = parse_sexpr(src, c, &body);
            SExpr::SLetIn(None, ident, Box::new(bindee), Box::new(body))
        }
        // sugarx <- ~(<sexpr>); <sexpr>
        "sletsample" => {
            tracing::debug!("parsing sletsample: {} >>> {}", n.to_sexp(), parse_str(src, &n));
            assert_children!(src, k, 3, n, c);

            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let ident = cs.next().unwrap();
            let utf8 = ident.utf8_text(src).unwrap();
            let ident = String::from_utf8(utf8.into()).unwrap();

            let bindee = cs.next().unwrap();
            let bindee = parse_sexpr(src, c, &bindee);

            let body = cs.next().unwrap();
            let body = parse_sexpr(src, c, &body);
            SExpr::SLetSample((), ident, Box::new(bindee), Box::new(body))
        }
        "site" => {
            tracing::debug!("parsing site: {} >>> {}", n.to_sexp(), parse_str(src, &n));
            assert_children!(src, k, 3, n, c);
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let pred = cs.next().unwrap();
            let pred = parse_sanf(src, c, pred);

            let tbranch = cs.next().unwrap();
            let tbranch = parse_sexpr(src, c, &tbranch);

            let fbranch = cs.next().unwrap();
            let fbranch = parse_sexpr(src, c, &fbranch);
            SExpr::SIte(None, Box::new(pred), Box::new(tbranch), Box::new(fbranch))
        }
        "sobserve" => {
            tracing::debug!("parsing sobs: {} >>> {}", n.to_sexp(), parse_str(src, &n));
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let aval = cs.next().unwrap();
            let aval = parse_sanf(src, c, aval);

            let adist = cs.next().unwrap();
            let adist = parse_sanf(src, c, adist);

            let body = cs.next().unwrap();
            let body = parse_sexpr(src, c, &body);

            SExpr::SObserve((), Box::new(aval), Box::new(adist), Box::new(body))
        }
        "sseq" => {
            tracing::debug!("parsing sseq: {} >>> {}", n.to_sexp(), parse_str(src, &n));
            assert_children!(src, k, 2, n, c);
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let e0 = cs.next().unwrap();
            let e0 = parse_sexpr(src, c, &e0);

            let e1 = cs.next().unwrap();
            let e1 = parse_sexpr(src, c, &e1);
            SExpr::SSeq((), Box::new(e0), Box::new(e1))
        }
        "ssample" => {
            tracing::debug!("parsing ssample: {} >>> {}", n.to_sexp(), parse_str(src, &n));
            assert_children!(src, k, 1, n, c);
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let distobj = cs.next().unwrap();
            let distobj = parse_sexpr(src, c, &distobj);

            SExpr::SSample((), Box::new(distobj))
        }
        "sexact" => {
            tracing::debug!("parsing sexact: {} >>> {}", n.to_sexp(), parse_str(src, &n));
            assert_children!(src, k, 1, n, c);
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let e = cs.next().unwrap();
            let e = parse_eexpr(src, c, &e);

            SExpr::SExact((), Box::new(e))
        }
        "sapp" => {
            tracing::debug!("parsing sapp: {} >>> {}", n.to_sexp(), parse_str(src, &n));
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let fnname = cs.next().unwrap();
            let fnname = parse_str(src, &fnname);

            let args = parse_vec_h(src, c, n, parse_sanf, 1);

            SExpr::SApp((), fnname, args)
        }
        "slambda" => {
            tracing::debug!("parsing slambda: {} >>> {}", n.to_sexp(), parse_str(src, &n));
            assert_children!(src, k, 2, n, c);
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let args = parse_vec(src, c, n, |a, b, c| parse_str(a, &c));

            let e = cs.next().unwrap();
            let e = parse_sexpr(src, c, &e);

            SExpr::SLambda((), args, Box::new(e))
        }
        "swhile" => {
            tracing::debug!("parsing swhile: {} >>> {}", n.to_sexp(), parse_str(src, &n));
            assert_children!(src, k, 2, n, c);
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let guard = cs.next().unwrap();
            let guard = parse_sanf(src, c, guard);

            let block = cs.next().unwrap();
            let block = parse_sexpr(src, c, &block);

            SExpr::SWhile((), Box::new(guard), Box::new(block))
        }
        "smap" => {
            tracing::debug!("parsing smap: {} >>> {}", n.to_sexp(), parse_str(src, &n));
            assert_children!(src, k, 5, n, c);
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let x = cs.next().unwrap();
            let x = parse_str(src, &x);

            let mapfn = cs.next().unwrap();
            let mapfn = parse_sexpr(src, c, &mapfn);

            let xs = cs.next().unwrap();
            let xs = parse_sanf(src, c, xs);

            SExpr::SMap((), x, Box::new(mapfn), Box::new(xs))
        }

        "sfold" => {
            tracing::debug!("parsing sfold: {} >>> {}", n.to_sexp(), parse_str(src, &n));
            assert_children!(src, k, 5, n, c);
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let init = cs.next().unwrap();
            let init = parse_sanf(src, c, init);

            let acc = cs.next().unwrap();
            let acc = parse_str(src, &acc);

            let x = cs.next().unwrap();
            let x = parse_str(src, &x);

            let foldfn = cs.next().unwrap();
            let foldfn = parse_sexpr(src, c, &foldfn);

            let xs = cs.next().unwrap();
            let xs = parse_sanf(src, c, xs);

            SExpr::SFold((), Box::new(init), acc, x, Box::new(foldfn), Box::new(xs))
        }
        s => panic!(
            "unexpected tree-sitter node kind `{}` (#named_children: {})! Likely, you need to rebuild the tree-sitter parser\nsexp: {}", s, n.named_child_count(), n.to_sexp()
        ),
    }
}

#[cfg(test)]
mod sampling_parser_tests {
    use super::*;
    use crate::parser::program::*;
    use crate::*;

    #[test]
    fn user_defined_functions() {
        use Anf::*;
        use EExpr::*;
        use ETy::*;
        use Program::*;
        use SExpr::*;
        use STy::*;
        let code = r#"
            exact fn foo (s1: Bool) -> Bool {
              bar
            }
            sample fn foo (s1: Bool) -> Bool {
              bar
            }
            sample {
              p <- poisson(0.4);
              exact { baz(p) }
            }
        "#;
        let expr = parse(code);
        let efun: Function<EExpr<Inferable>> = Function {
            name: Some("foo".to_string()),
            arguments: [AVar(Some(EBool), "s1".to_string())].to_vec(),
            body: EAnf((), Box::new(AVar(None, "bar".to_string()))),
            returnty: EBool,
        };
        let sfun: Function<SExpr<Inferable>> = Function {
            name: Some("foo".to_string()),
            arguments: [AVar(Some(SBool), "s1".to_string())].to_vec(),
            body: SAnf((), Box::new(AVar(None, "bar".to_string()))),
            returnty: SBool,
        };
        let prg = SLetIn(
            None,
            "p".to_string(),
            Box::new(SAnf(
                (),
                Box::new(AnfPoisson((), Box::new(AVal((), SVal::SFloat(0.4))))),
            )),
            Box::new(SExact(
                (),
                Box::new(EApp(
                    None,
                    "baz".to_string(),
                    [AVar(None, "p".to_string())].to_vec(),
                )),
            )),
        );
        assert_eq!(
            expr.unwrap(),
            Program::EDefine(efun, Box::new(Program::SDefine(sfun, Box::new(SBody(prg)))))
        );
    }

    #[test]
    fn function_call() {
        let code = r#"
        sample { apply(0, 0, 0, 0, 0) }
        "#;
        let expr = parse(code);
        let zero = Anf::AVal((), SVal::SInt(0));
        let call = SExpr::SApp(
            (),
            "apply".to_string(),
            vec![
                zero.clone(),
                zero.clone(),
                zero.clone(),
                zero.clone(),
                zero.clone(),
            ],
        );

        assert_eq!(expr.unwrap(), Program::SBody(call));
    }
}
