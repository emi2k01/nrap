use std::fmt;

pub type RuntimeResult<T> = std::result::Result<T, RuntimeError>;

#[derive(Debug, Clone)]
pub enum RuntimeError {
    Generic(String),
    OutParamNoIdent,
    IndexNotANumber,
    NonProcedureInTopLevel,
    IfConditionNotABool,
    BreakIfConditionNotABool,
    ProcedureCallExprNotValid,
    InfixOpWithInvalidTypes,
    PrefixOpWithInvalidType,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Error: ")?;
        match self {
            RuntimeError::Generic(string) => write!(f, "{}", string),
            RuntimeError::OutParamNoIdent =>
                write!(f, "Tried to use an invalid expression in «out» argument.\
                 «Out» argument must be an identifier"),
            RuntimeError::IndexNotANumber =>
                write!(f, "Tried to use an invalid expression in index operation.\
                 And index must be a number"),
            RuntimeError::NonProcedureInTopLevel =>
                write!(f, "Tried to use a non-procedure statement in the top level of the program.\
                 All non-procedure statements must be inside a procedure"),
            RuntimeError::IfConditionNotABool =>
                write!(f, "Tried to use an invalid expression in «if» statement.\
                 «if» conditions must be bool expressions"),
            RuntimeError::BreakIfConditionNotABool =>
                write!(f, "Tried to use an invalid expression in «break if» statement.\
                 «break if» conditions must be bool expressions"),
            RuntimeError::ProcedureCallExprNotValid =>
                write!(f, "Tried to use an invalid procedure in a call expression.\
                 To use a procedure in a call statement, it must have only one «out» parameter and it must be the last one"),
            RuntimeError::InfixOpWithInvalidTypes =>
                write!(f, "Tried to use an infix operation with invalid types."),
            RuntimeError::PrefixOpWithInvalidType =>
                write!(f, "Tried to use a prefix operation with an invalid type"),
        }
    }
}
