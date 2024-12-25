// use super::ttg::*;
use super::classes::*;
use super::exact::*;
use super::function::*;
use super::sampling::*;
use super::ttg::*;
use std::fmt::Debug;

crate::TTG!(
    #[derive(Debug, Clone, PartialEq)]
    pub enum Program<X> {
        EBody(EExpr<X>),
        SBody(SExpr<X>),
        EDefine(Function<EExpr<X>>, Box<Program<X>>),
        SDefine(Function<SExpr<X>>, Box<Program<X>>),
    }
);

crate::TTG!(
    #[derive(Debug, Clone, PartialEq)]
    pub enum Query<X> {
        EQuery(EExpr<X>),
        SQuery(SExpr<X>),
    }
);

crate::TTG!(
    impl<X> Program<X> {
        pub fn query(&self) -> Query<X> {
            use Program::*;
            match self {
                EBody(e) => Query::EQuery(e.query()),
                SBody(e) => Query::SQuery(e.query()),
                EDefine(f, p) => p.query(),
                SDefine(f, p) => p.query(),
            }
        }
        pub fn insert_observe(&self, e: EExpr<X>) -> Program<X> {
            use Program::*;
            match self {
                EBody(b) => EBody(b.insert_observe(e)),
                SBody(b) => todo!(), //  Body(b.insert_observe(e)),
                EDefine(f, p) => EDefine(f.clone(), Box::new(p.insert_observe(e))),
                SDefine(f, p) => SDefine(f.clone(), Box::new(p.insert_observe(e))),
            }
        }
    }
);
