pub mod reporting;
use crate::compiler::backend;
use crate::compiler::frontend;
use crate::compiler::source::registry;
use thiserror::Error;

/// Common Error type for all kinds of compiler errors
/// The compiler phases usually add their own specific error types,
/// which are then unified under this.
///
/// Users of the `Compiler` should only have to deal with this `Error` type,
/// whereas internals might deal with specific kinds.
///
/// See also `error::reporting`, which is used to create nicely formatted
/// error messages from this type.
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
