#[derive(Clone, Eq, Hash, PartialEq, Debug)]
pub enum CompileError {
    AcceptingNonZeroError(String),
    Todo(),
    TypeError(String),
    Generic(String),
    SemanticsError(String),
}
impl CompileError {
    pub fn to_string(&self) -> String {
        use CompileError::*;
        match self {
            AcceptingNonZeroError(s) => s.to_string(),
            Todo() => "todo!".to_string(),
            TypeError(s) => s.to_string(),
            Generic(s) => s.to_string(),
            SemanticsError(s) => s.to_string(),
        }
    }
}

pub type Result<T> = core::result::Result<T, CompileError>;
