use crate::typeinf::grammar::Inferable;
use crate::*;
use std::fmt::Debug;
use std::str::FromStr;
use tracing::*;
use tree_sitter::{Node, Parser, Tree, TreeCursor};

pub fn tree_parser(code: String) -> Option<Tree> {
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_yodel::language())
        .expect("Error loading yodel grammar");
    parser.parse(code, None)
}
// fn parse_program(src: &[u8], c: &mut TreeCursor, n: &Node) -> ProgramSugar {
//     ProgramSugar::Exact(parse_expr(src, c, n))
// }
// pub fn parse(code: &str) -> Option<ProgramSugar> {
//     let tree = tree_parser(code.to_string())?;
//     let expr = parse_tree(code.as_bytes(), tree);
//     Some(expr)
// }
pub fn parse_str(src: &[u8], n: &Node) -> String {
    let utf8 = n.clone().utf8_text(src).unwrap();
    String::from_utf8(utf8.into()).unwrap()
}

#[macro_export]
macro_rules! parse_todo {
    ( $src:expr, $n:expr ) => {{
        todo!(
            "\nkind: {}\nsexp: {}\nsrc: {}\n",
            $n.kind(),
            $n.to_sexp(),
            parse_str($src, &$n)
        )
    }};
}

pub fn parse_vec_h<X>(
    src: &[u8],
    c: &mut TreeCursor,
    n: Node,
    parse_el: impl Fn(&[u8], &mut TreeCursor, Node) -> X,
    nshifts: usize,
) -> Vec<X> {
    trace!("parse_vec: {}", parse_str(src, &n));
    trace!("     sexp: {}", n.to_sexp());
    let mut xs = vec![];
    let mut _c = c.clone();
    let mut cs = n.named_children(&mut _c);
    let mut sft = nshifts;
    while sft > 0 {
        cs.next();
        sft -= 1;
    }

    for i in 0..(n.named_child_count() - nshifts) {
        let x = cs.next().unwrap();
        trace!("arg[{}]: {}", i, x.to_sexp());
        let x = parse_el(src, c, x);
        xs.push(x);
    }
    xs
}

pub fn parse_vec<X>(
    src: &[u8],
    c: &mut TreeCursor,
    n: Node,
    parse_el: impl Fn(&[u8], &mut TreeCursor, Node) -> X,
) -> Vec<X> {
    parse_vec_h(src, c, n, parse_el, 0)
}

pub fn parse_num<T: FromStr>(src: &[u8], n: &Node) -> T
where
    <T as FromStr>::Err: Debug,
{
    let utf8 = n.utf8_text(src).unwrap();
    let s = String::from_utf8(utf8.into()).unwrap();
    s.parse::<T>().unwrap()
}

// generic values
#[derive(Debug, Clone, PartialEq)]
pub enum GVal {
    Bool(bool),
    Float(f64),
    Int(u64),
}

pub fn parse_gval(src: &[u8], c: &mut TreeCursor, n: &Node) -> Option<GVal> {
    match n.kind() {
        "bool" => {
            let utf8 = n.utf8_text(src).unwrap();
            let var = String::from_utf8(utf8.into()).unwrap();
            let b = if var == *"true" {
                true
            } else if var == *"false" {
                false
            } else {
                panic!("impossible")
            };
            Some(GVal::Bool(b))
        }
        "int" => Some(GVal::Int(parse_num(src, n))),
        "float" => Some(GVal::Float(parse_num(src, n))),
        _ => None,
    }
}

// pub fn parse_anf<Val>(src: &[u8], c: &mut TreeCursor, n: Node, parse_anf_node: impl Fn(&[u8], &mut TreeCursor, Node)->Anf<Inferable, Val>) -> Anf<Inferable, Val>
//     where
//       Val: PartialEq + Clone + Debug,
//       AValExt<Val>: ξ<Inferable>,
//       AVarExt<Val>: ξ<Inferable>,
//       <ttg::AValExt<Val> as ξ<Inferable>>::Ext: PartialEq + Clone + Debug,
//       <ttg::AVarExt<Val> as ξ<Inferable>>::Ext: PartialEq + Clone + Debug,
// {
//     match n.named_child_count() {
//         0 => parse_anf_node(src, c, n), // parse that node!
//         1 => {
//             // found an extra paren, unwrap and continue
//             let mut c_ = c.clone();
//             let mut cs = n.named_children(&mut c_);
//             let a = cs.next().unwrap();
//             parse_anf(src, c, n, parse_anf_node)
//         },
//         2 => {
//             // unary operation + dist constructors
//             let mut _c = c.clone();
//             let mut cs = n.named_children(&mut _c);
//             let op = cs.next().unwrap();
//             let utf8 = op.utf8_text(src).unwrap();
//             let op = String::from_utf8(utf8.into()).unwrap();

//             let a = cs.next().unwrap();
//             let anf = parse_anf(src, c, a, parse_anf_node);
//             match op.as_str() {
//                 "!" => Anf::Neg(Box::new(anf)),
//                 "eflip" => Anf::EFlip(Box::new(anf)),
//                 "sbern" => Anf::SBern(Box::new(anf)),
//                 "spoisson" => Anf::SPoisson(Box::new(anf)),
//                 "sdirichlet" => Anf::SDirichlet(parse_vec(src, c, anf, |a, b, c| parse_anf_node(a, b,c))),
//                 "sdiscrete" => Anf::SDiscrete(parse_vec(src, c, anf, |a, b, c| parse_anf_node(a, b,c))),
//                 _ => panic!("invalid unary operator found!\nsexp: {}", n.to_sexp())
//             }
//         }
//         3 => {
//             // binary operation
//             let mut _c = c.clone();
//             let mut cs = n.named_children(&mut _c);

//             let l = cs.next().unwrap();
//             let l = parse_anf(src, c, l, parse_anf_node);

//             let op = cs.next().unwrap();
//             let utf8 = op.utf8_text(src).unwrap();
//             let op = String::from_utf8(utf8.into()).unwrap();

//             let r = cs.next().unwrap();
//             let r = parse_anf(src, c, r, parse_anf_node);
//             match op.as_str() {
//                 "&&" => Anf::And(Box::new(l), Box::new(r)),
//                 "||" => Anf::Or(Box::new(l), Box::new(r)),

//                 "/" => Anf::Div(Box::new(l), Box::new(r)),
//                 "*" => Anf::Mult(Box::new(l), Box::new(r)),
//                 "+" => Anf::Plus(Box::new(l), Box::new(r)),
//                 "-" => Anf::Minus(Box::new(l), Box::new(r)),

//                 "<" => Anf::LT(Box::new(l), Box::new(r)),
//                 "<=" => Anf::LTE(Box::new(l), Box::new(r)),
//                 ">" => Anf::GT(Box::new(l), Box::new(r)),
//                 ">=" => Anf::GTE(Box::new(l), Box::new(r)),
//                 "==" => Anf::EQ(Box::new(l), Box::new(r)),

//                 /// fixme, these are not binary infix operators
//                 "suniform" => Anf::SUniform(Box::new(l), Box::new(r))
//                 "snormal" => Anf::SNormal(Box::new(l), Box::new(r))
//                 "sbeta" => Anf::SBeta(Box::new(l), Box::new(r))
//                 "sprj" => Anf::SPrj(Box::new(l), Box::new(r)) // FIXME: this is (sprj) (indent) (anf)
//                 _ => panic!("invalid binary operator found!\nsexp: {}", n.to_sexp()),
//             }
//         }
//         _ => panic!("invalid a-normal form found!\nsexp: {}", n.to_sexp()),
//     }
// }
