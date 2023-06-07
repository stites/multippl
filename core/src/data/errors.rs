use std::fmt;

#[derive(Clone, Eq, Hash, PartialEq, Debug)]
pub enum CompileError {
    AcceptingNonZeroError(String),
    Todo(),
    TypeError(String),
    Generic(String),
    SemanticsError(String),
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
        }
    }
}

pub type Result<T> = core::result::Result<T, CompileError>;
