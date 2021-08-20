use super::code_generator;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum Error {
    #[error(transparent)]
    CodeGenerator(#[from] code_generator::Error),
}
