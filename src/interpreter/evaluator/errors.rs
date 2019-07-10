pub type RuntimeResult<T> = std::result::Result<T, RuntimeError>;

#[derive(Debug, Clone)]
pub enum RuntimeError {
    Generic(String),
}
