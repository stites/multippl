use std::fmt;

#[derive(Clone, Eq, Hash, PartialEq, Debug)]
pub enum CompileError {
    AcceptingNonZeroError(String),
    Todo(),
    TypeError(String),
    Generic(String),
    SemanticsError(String),
    ErasedInAbovePass(),
    InexpressibleExactExpr(),
    InexpressibleSampleExpr(),
}
impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use CompileError::*;
        match self {
            AcceptingNonZeroError(s) => write!(f, "{}", s),
            Todo() => write!(f, "todo!"),
            TypeError(s) => write!(f, "{}", s),
            Generic(s) => write!(f, "{}", s),
            SemanticsError(s) => write!(f, "{}", s),
            ErasedInAbovePass() => write!(f, "erased in above pass"),
            InexpressibleExactExpr() => {
                write!(f, "expression is not expressible in the exact language")
            }
            InexpressibleSampleExpr() => {
                write!(f, "expression is not expressible in the sample language")
            }
        }
    }
}

pub type Result<T> = core::result::Result<T, CompileError>;

pub fn non_zero<T>(s: &str) -> Result<T> {
    Err(CompileError::AcceptingNonZeroError(s.to_string()))
}
pub fn generic<T>(s: &str) -> Result<T> {
    Err(CompileError::Generic(s.to_string()))
}
pub fn typecheck_failed<T>(ctx: &str) -> Result<T> {
    Err(CompileError::TypeError(format!(
        "impossible: typechecking failed in earlier stage of pipeline. Context: {ctx}"
    )))
}
#[allow(non_snake_case)]
pub fn TODO<T>() -> Result<T> {
    Err(CompileError::Todo())
}
pub fn semantics<T>(s: &str) -> Result<T> {
    Err(CompileError::SemanticsError(s.to_string()))
}
pub fn erased<T>() -> Result<T> {
    Err(CompileError::ErasedInAbovePass())
}
pub fn not_in_exact<T>() -> Result<T> {
    Err(CompileError::InexpressibleExactExpr())
}
pub fn not_in_sample<T>() -> Result<T> {
    Err(CompileError::InexpressibleSampleExpr())
}
