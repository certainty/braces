use super::code_generator;
use thiserror::Error;

#[derive(Error)]
pub enum Error {
    #[error(transparent)]
    CodeGenerator(#[from] code_generator::Error),
}
