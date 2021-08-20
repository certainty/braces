pub mod reporting;
use crate::compiler::backend;
use crate::compiler::frontend;

#[derive(Debug)]
pub enum Error {
    FrontendError(frontend::error::Error),
    BackendError(backend::error::Error),
}
