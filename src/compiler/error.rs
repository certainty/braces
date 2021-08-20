pub mod reporting;
use crate::compiler::backend;
use crate::compiler::frontend;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    FrontendError(#[from] frontend::error::Error),

    #[error(transparent)]
    BackendError(#[from] backend::error::Error),
}
