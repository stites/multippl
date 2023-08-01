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
    // top level forms:
    match n.kind() {
        "sanfprod" => Anf::AnfProd(parse_vec(src, c, n, |a, b, c| parse_sanf(a, b, c))),
        "sanfprj" => {
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let ident = cs.next().unwrap();
            let ident = Anf::AVar(None, parse_str(src, &ident));

            let prj = cs.next().unwrap();
            let prj = parse_sanf(src, c, prj);
            Anf::AnfPrj(Box::new(ident), Box::new(prj))
        }
        "svalue" => {
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);
            let node = cs.next().unwrap();

            Anf::AVal((), parse_sval(src, c, &node))
        }
        kind => {
            tracing::debug!("kind: {kind}, #children: {}", n.named_child_count());
            match n.named_child_count() {
                0 => {
                    tracing::debug!("parsing anf 0: {} >>> {}", n.to_sexp(), parse_str(src, &n));
                    match kind {
                        "identifier" => {
                            let utf8 = n.utf8_text(src).unwrap();
                            let var = String::from_utf8(utf8.into()).unwrap();
                            Anf::AVar(None, var)
                        }
                        _ => {
                            if kind == "bool" || kind == "float" || kind == "int" {
                                let sval = parse_sval(src, c, &n);
                                Anf::AVal((), sval)
                            } else {
                                panic!("invalid anf! found sexp:\n{}", n.to_sexp())
                            }
                        }
                    }
                }
                1 => {
                    // unwrap an svalue, or an anf wrapped with extra parens
                    // println!("parse 1: {}", n.to_sexp());
                    let mut _c = c.clone();
                    let mut cs = n.named_children(&mut _c);
                    let node = cs.next().unwrap();

                    match node.kind() {
                        "sanf" => parse_sanf(src, c, node),
                        "identifier" => Anf::AVar(None, parse_str(src, &node)),
                        "svalue" => Anf::AVal((), parse_sval(src, c, &node)),
                        "sanfprod" => {
                            Anf::AnfProd(parse_vec(src, c, node, |a, b, c| parse_sanf(a, b, c)))
                        }
                        "!" => Anf::Neg(Box::new(parse_sanf(src, c, node))),
                        "sanfbern" => Anf::AnfBernoulli((), Box::new(parse_sanf(src, c, node))),
                        "sanfpoisson" => Anf::AnfPoisson((), Box::new(parse_sanf(src, c, node))),
                        "sanfdirichlet" => Anf::AnfDirichlet(
                            (),
                            parse_vec(src, c, node, |a, b, c| parse_sanf_node(a, b, &c)),
                        ),
                        "sanfdiscrete" => Anf::AnfDiscrete(
                            (),
                            parse_vec(src, c, node, |a, b, c| parse_sanf_node(a, b, &c)),
                        ),
                        _ => panic!("invalid unary operator found!\nsexp: {}", n.to_sexp()),
                    }
                }
                2 => {
                    // // unary operation
                    // let mut _c = c.clone();
                    // let mut cs = n.named_children(&mut _c);
                    // let op = cs.next().unwrap();
                    // let utf8 = op.utf8_text(src).unwrap();
                    // let op = String::from_utf8(utf8.into()).unwrap();

                    let mut _c = c.clone();
                    let mut cs = n.named_children(&mut _c);
                    let node = cs.next().unwrap();
                    tracing::debug!(
                        "parsing anf 2: (kind {}) {} {}",
                        node.kind(),
                        n.to_sexp(),
                        parse_str(src, &n)
                    );
                    match node.kind() {
                        "sanf" => parse_sanf(src, c, node),
                        "identifier" => Anf::AVar(None, parse_str(src, &node)),
                        "bool_unop" => {
                            let node = cs.next().unwrap();
                            Anf::Neg(Box::new(parse_sanf(src, c, node)))
                        }
                        _ => panic!("invalid unary operator found!\nsexp: {}", n.to_sexp()),
                    }
                }
                3 => {
                    // println!("parse 3: {}", n.to_sexp());
                    // binary operation
                    let mut _c = c.clone();
                    let mut cs = n.named_children(&mut _c);

                    let c0 = cs.next().unwrap();
                    let prefixop = parse_str(src, &c0);
                    let isprefix_dist = match prefixop.as_str() {
                        "sprj" => Some(false),
                        "sanfuniform" => Some(true),
                        "sanfnormal" => Some(true),
                        "sanfbeta" => Some(true),
                        _ => None,
                    };
                    if isprefix_dist.is_none() {
                        let c0 = parse_sanf(src, c, c0);

                        let c1 = cs.next().unwrap();
                        let utf8 = c1.utf8_text(src).unwrap();
                        let infix_str = String::from_utf8(utf8.into()).unwrap();

                        let c2 = cs.next().unwrap();
                        let c2 = parse_sanf(src, c, c2);

                        match infix_str.as_str() {
                            "&&" => Anf::And(Box::new(c0), Box::new(c2)),
                            "||" => Anf::Or(Box::new(c0), Box::new(c2)),

                            "/" => Anf::Div(Box::new(c0), Box::new(c2)),
                            "*" => Anf::Mult(Box::new(c0), Box::new(c2)),
                            "+" => Anf::Plus(Box::new(c0), Box::new(c2)),
                            "-" => Anf::Minus(Box::new(c0), Box::new(c2)),

                            "<" => Anf::LT(Box::new(c0), Box::new(c2)),
                            "<=" => Anf::LTE(Box::new(c0), Box::new(c2)),
                            ">" => Anf::GT(Box::new(c0), Box::new(c2)),
                            ">=" => Anf::GTE(Box::new(c0), Box::new(c2)),
                            "==" => Anf::EQ(Box::new(c0), Box::new(c2)),
                            _ => panic!("invalid binary operator found!\nsexp: {}", n.to_sexp()),
                        }
                    } else if isprefix_dist == Some(true) {
                        let c1 = cs.next().unwrap();
                        let c1 = parse_sanf(src, c, c1);

                        let c2 = cs.next().unwrap();
                        let c2 = parse_sanf(src, c, c2);
                        match prefixop.as_str() {
                            "sanfuniform" => Anf::AnfUniform((), Box::new(c1), Box::new(c2)),
                            "sanfnormal" => Anf::AnfNormal((), Box::new(c1), Box::new(c2)),
                            "sanfbeta" => Anf::AnfBeta((), Box::new(c1), Box::new(c2)),
                            _ => panic!("invalid binary operator found!\nsexp: {}", n.to_sexp()),
                        }
                    } else if isprefix_dist == Some(false) {
                        match prefixop.as_str() {
                            "sanfprj" => {
                                // let c1 = cs.next().unwrap();
                                // let utf8 = c1.utf8_text(src).unwrap();
                                // let arr_str = String::from_utf8(utf8.into()).unwrap();

                                let c1 = cs.next().unwrap();
                                let c1 = parse_sanf(src, c, c1);

                                let c2 = cs.next().unwrap();
                                let c2 = parse_sanf(src, c, c2);
                                Anf::AnfPrj(Box::new(c1), Box::new(c2))
                            }
                            _ => panic!("invalid operator found!\nsexp: {}", n.to_sexp()),
                        }
                    } else {
                        panic!("incomplete function is impossible")
                    }
                }

                k => {
                    // try again
                    let mut _c = c.clone();
                    let mut cs = n.named_children(&mut _c);
                    let node = cs.next().unwrap();
                    match node.kind() {
                        // "eanfprod" => {
                        //     tracing::debug!("caught a prod");
                        //     Anf::AnfProd(parse_vec(src, c, node, |a, b, c| parse_anf(a, b, c)))
                        // }
                        _ => panic!(
                            "invalid program found with {k} children!\nsexp: {}",
                            n.to_sexp()
                        ),
                    }
                }
            }
        }
    }
}

pub fn parse_sval(src: &[u8], c: &mut TreeCursor, n: &Node) -> SVal {
    use Dist::*;
    match shared::parse_gval(src, c, n) {
        Some(GVal::Bool(x)) => SVal::SBool(x),
        Some(GVal::Float(x)) => SVal::SFloat(x),
        Some(GVal::Int(x)) => SVal::SInt(x),
        None => match n.kind() {
            "svalue" => {
                let mut _c = c.clone();
                let mut cs = n.named_children(&mut _c);

                let n = cs.next().unwrap();
                parse_sval(src, c, &n)
            }
            "svec" => SVal::SVec(parse_vec(src, c, *n, |a, b, c| parse_sval(a, b, &c))),
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
            "sdiscrete" => SVal::SDist(Discrete(parse_vec(src, c, *n, |a, b, c| parse_num(a, &c)))),
            "sdirichlet" => {
                SVal::SDist(Dirichlet(parse_vec(src, c, *n, |a, b, c| parse_num(a, &c))))
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
        "tyProd" => {
            STy::SProd(parse_vec(src, c, n, |a, b, c| parse_stype(a, b, &c)))
        }
        s => panic!(
            "unexpected tree-sitter type (kind `{}`) (#named_children: {})! Likely, you need to rebuild the tree-sitter parser\nsexp: {}", s, n.named_child_count(), n.to_sexp()
        ),
    }
}
pub fn parse_sanf_node(src: &[u8], c: &mut TreeCursor, n: &Node) -> Anf<Inferable, SVal> {
    match n.kind() {
        "identifier" => Anf::AVar(None, parse_str(src, &n)),
        "svalue" => {
            // println!("parse_sanf_node: svalue");
            Anf::AVal((), parse_sval(src, c, &n))
        }
        _ => {
            todo!("parse_sanf_node, not ready for: {}", n.kind())
        }
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
            tracing::debug!("parsing sobserve: {} >>> {}", n.to_sexp(), parse_str(src, &n));
            let a0 = parse_sanf(src, c, n);
            let a1 = parse_sanf(src, c, n);
            SExpr::SObserve((), Box::new(a0), Box::new(a1))
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
            assert_children!(src, k, 2, n, c);
            let mut _c = c.clone();
            let mut cs = n.named_children(&mut _c);

            let fnname = cs.next().unwrap();
            let fnname = parse_str(src, &fnname);

            let args = parse_vec(src, c, n, |a, b, c| parse_sanf(a, b, c));

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
            exact fn foo (s1: Bool) : Bool {
              bar
            }
            sample fn foo (s1: Bool) : Bool {
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
                    [AVar(None, "baz".to_string()), AVar(None, "p".to_string())].to_vec(),
                )),
            )),
        );
        assert_eq!(
            expr.unwrap(),
            Program::EDefine(efun, Box::new(Program::SDefine(sfun, Box::new(SBody(prg)))))
        );
    }
}
