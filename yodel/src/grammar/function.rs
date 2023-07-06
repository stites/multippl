use super::classes::*;
use std::fmt::Debug;

#[derive(PartialEq, Debug, Clone)]
pub struct Function<Expr: Lang>
where
    <Expr as Lang>::Anf: PartialEq + Debug + Clone,
    <Expr as Lang>::Ty: PartialEq + Debug + Clone,
    Expr: PartialEq + Debug + Clone,
{
    pub name: Option<String>,
    pub arguments: Vec<<Expr as Lang>::Anf>,
    pub body: Expr,
    pub returnty: <Expr as Lang>::Ty,
}
