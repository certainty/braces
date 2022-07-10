use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum Error {
    #[error("Too many locals defined")]
    TooManyLocals,
    #[error("Too many up values defined")]
    TooManyUpValues,
    #[error("CompilerBug: {}", 0)]
    CompilerBug(String),
}
