use super::exact::{parse_eexpr, parse_etype};
use super::sampling::{parse_sexpr, parse_stype};
use super::shared::*;
use crate::typeinf::grammar::Inferable;
use crate::*;
use tree_sitter::{Node, Tree, TreeCursor};

pub fn parse_earg(src: &[u8], c: &mut TreeCursor, n: &Node) -> Anf<Inferable, EVal> {
    // println!("{}: {}", n.kind(), n.to_sexp());
    let mut c_ = c.clone();
    let mut cs = n.named_children(&mut c_);

    let argname = cs.next().unwrap();
    // println!("argname: {}", argname.to_sexp());
    let argname = parse_str(src, &argname);
    // println!("argname: {}", argname);

    match cs.next() {
        Some(ty) => {
            let ty = parse_etype(src, c, &ty);
            Anf::AVar(ty, argname)
        }
        None => Anf::AVar(None, argname),
    }
}
pub fn parse_eargs(src: &[u8], c: &mut TreeCursor, n: &Node) -> Vec<Anf<Inferable, EVal>> {
    // println!("eargs: {}", n.to_sexp());
    parse_vec(src, c, *n, |src, c, node| parse_earg(src, c, &node))
}

pub fn parse_efunction(src: &[u8], c: &mut TreeCursor, n: &Node) -> Function<EExpr<Inferable>> {
    // println!("efun: {}", n.to_sexp());
    let mut c_ = c.clone();
    let mut cs = n.named_children(&mut c_);

    let name = cs.next().unwrap();
    let name = parse_str(src, &name);

    let arguments = cs.next().unwrap();
    let arguments = parse_eargs(src, c, &arguments);

    let returntysrc = cs.next().unwrap();
    let returnty = parse_etype(src, c, &returntysrc);
    let body = match &returnty {
        None => parse_eexpr(src, c, &returntysrc),
        Some(rt) => {
            let body = cs.next().unwrap();
            parse_eexpr(src, c, &body)
        }
    };

    Function {
        name: Some(name),
        arguments,
        body,
        returnty,
    }
}

pub fn parse_sarg(src: &[u8], c: &mut TreeCursor, n: &Node) -> Anf<Inferable, SVal> {
    let mut c_ = c.clone();
    let mut cs = n.named_children(&mut c_);

    let argname = cs.next().unwrap();
    let argname = parse_str(src, &argname);

    match cs.next() {
        Some(ty) => {
            let ty = parse_stype(src, c, &ty);
            Anf::AVar(ty, argname)
        }
        None => Anf::AVar(None, argname),
    }
}
pub fn parse_sargs(src: &[u8], c: &mut TreeCursor, n: &Node) -> Vec<Anf<Inferable, SVal>> {
    parse_vec(src, c, *n, |src, c, node| parse_sarg(src, c, &node))
}

pub fn parse_sfunction(src: &[u8], c: &mut TreeCursor, n: &Node) -> Function<SExpr<Inferable>> {
    let mut c_ = c.clone();
    let mut cs = n.named_children(&mut c_);

    let name = cs.next().unwrap();
    let name = parse_str(src, &name);

    let arguments = cs.next().unwrap();
    let arguments = parse_sargs(src, c, &arguments);

    let returntysrc = cs.next().unwrap();
    let returnty = parse_stype(src, c, &returntysrc);
    let body = match &returnty {
        None => parse_sexpr(src, c, &returntysrc),
        Some(rt) => {
            let body = cs.next().unwrap();
            parse_sexpr(src, c, &body)
        }
    };

    Function {
        name: Some(name),
        arguments,
        body,
        returnty,
    }
}
