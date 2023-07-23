pub mod exact; // discrete
pub mod sample; // let-sample-in

use self::exact::*;
use self::sample::*;
use crate::*;
use std::fmt::Debug;

fn desugar_fun<ExprIn, ExprOut, Val, T>(
    translate_expr: impl Fn(&ExprIn) -> Result<ExprOut>,
    f: &Function<ExprIn>,
) -> Result<Function<ExprOut>>
where
    <ExprIn as Lang>::Anf: PartialEq + Debug + Clone,
    <ExprOut as Lang>::Anf: PartialEq + Debug + Clone,
    <ExprIn as Lang>::Ty: PartialEq + Debug + Clone,
    <ExprOut as Lang>::Ty: PartialEq + Debug + Clone,
    ExprIn: PartialEq + Debug + Clone + Lang<Anf = Anf<UD, Val>, Ty = T>,
    ExprOut: PartialEq + Debug + Clone + Lang<Anf = Anf<UD, Val>, Ty = T>,
    AVarExt<Val>: ξ<UD, Ext = ()>,
    // APrjExt<Val>: ξ<UD, Ext = ()>,
    ADistExt<Val>: ξ<UD, Ext = ()>,
    AValExt<Val>: ξ<UD, Ext = ()>,
    Val: Debug + PartialEq + Clone,
{
    Ok(Function {
        name: f.name.clone(),
        arguments: f.arguments.clone(),
        body: translate_expr(&f.body)?,
        returnty: f.returnty.clone(),
    })
}

pub fn desugar(p: &grammar::ProgramUD) -> Result<ProgramUD> {
    use Program::*;
    match p {
        EBody(e) => Ok(EBody(desugar_eexpr(e)?)),
        SBody(e) => Ok(SBody(desugar_sexpr(e)?)),
        EDefine(f, p) => Ok(EDefine(
            desugar_fun(desugar_eexpr, f)?,
            Box::new(desugar(p)?),
        )),
        SDefine(f, p) => Ok(SDefine(
            desugar_fun(desugar_sexpr, f)?,
            Box::new(desugar(p)?),
        )),
    }
}
