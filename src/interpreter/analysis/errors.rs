use std::fmt;

pub type AnalysisResult<T> = std::result::Result<T, AnalysisError>;

#[derive(Debug, Clone)]
pub enum AnalysisError {
    Generic,
}

impl fmt::Display for AnalysisError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use AnalysisError::*;
        match &self {
            Generic => write!(f, "There's an error in your program"),
        }
    }
}
