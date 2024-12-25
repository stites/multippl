use super::classes::*;
use crate::annotate::grammar::NamedVar;
use crate::data::errors::Result;
use std::fmt::Debug;

#[derive(PartialEq, Debug, Clone)]
pub struct Function<Expr: Lang>
where
    <Expr as Lang>::Anf: PartialEq + Debug + Clone,
    <Expr as Lang>::Ty: PartialEq + Debug + Clone,
    Expr: PartialEq + Debug + Clone,
{
    pub name: Option<String>,
    pub arguments: Vec<<Expr as Lang>::Anf>, // should just be a string
    pub body: Expr,
    pub returnty: <Expr as Lang>::Ty,
}

impl<Expr> Function<Expr>
where
    <Expr as Lang>::Anf: PartialEq + Debug + Clone,
    <Expr as Lang>::Ty: PartialEq + Debug + Clone,
    Expr: Lang + PartialEq + Debug + Clone,
{
    pub fn args2vars(
        &self,
        f: impl Fn(&<Expr as Lang>::Anf) -> Result<NamedVar>,
    ) -> Result<Vec<NamedVar>> {
        self.arguments.iter().map(f).collect()
    }
}
