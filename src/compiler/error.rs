pub mod reporting;
use crate::compiler::backend;
use crate::compiler::frontend;
use crate::compiler::source::registry;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    SourceCodeRegistryError(#[from] registry::Error),

    #[error("FrontendError")]
    FrontendError(#[from] frontend::error::Error),

    #[error(transparent)]
    BackendError(#[from] backend::error::Error),

    #[error(transparent)]
    IoError(#[from] std::io::Error),
}
