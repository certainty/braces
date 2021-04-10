use super::Input;
use crate::compiler::source_location::SourceLocation;
use nom::error::{VerboseError, VerboseErrorKind};
use nom::Err;
use thiserror::Error;

type NomError<'a> = VerboseError<Input<'a>>;

#[derive(Debug, Clone)]
pub struct ParserErrorDetail {
    pub context: String,
    pub span: String,
    pub location: SourceLocation,
}

impl<'a> From<&(Input<'a>, VerboseErrorKind)> for ParserErrorDetail {
    fn from(e: &(Input<'a>, VerboseErrorKind)) -> ParserErrorDetail {
        let (input, kind) = e;
        let context = match kind {
            VerboseErrorKind::Context(ctx) => ctx.to_string(),
            VerboseErrorKind::Char(c) => format!("expected {}", c),
            VerboseErrorKind::Nom(k) => format!("while parsing {:?}", k),
        };
        let span = input.fragment().to_string();
        let location = SourceLocation::new(
            input.extra.clone(),
            input.location_line() as usize,
            input.get_column() as usize,
        );

        ParserErrorDetail {
            context,
            span,
            location,
        }
    }
}

#[derive(Error, Debug)]
pub enum ReadError {
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error("ReadError: {}", 0)]
    ReadError(Vec<ParserErrorDetail>),
    #[error("Incomplete Input")]
    IncompleteInput,
}

impl<'a> From<Err<NomError<'a>>> for ReadError {
    fn from(e: Err<NomError<'a>>) -> ReadError {
        match e {
            Err::Incomplete(_) => ReadError::IncompleteInput,
            Err::Failure(e) => {
                ReadError::ReadError(e.errors.iter().map(|elt| elt.into()).collect())
            }
            Err::Error(e) => ReadError::ReadError(e.errors.iter().map(|elt| elt.into()).collect()),
        }
    }
}
