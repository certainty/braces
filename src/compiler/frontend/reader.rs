pub mod sexp;
use crate::compiler::source::Source;
use crate::compiler::source_location::SourceLocation;
use nom::error::{VerboseError, VerboseErrorKind};
use nom::Err;
use sexp::datum::Datum;
use sexp::Input;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error("ReadError: {}", 0)]
    ReadError(Vec<ParserErrorDetail>),
    #[error("Incomplete Input")]
    IncompleteInput,
}

type NomError<'a> = VerboseError<Input<'a>>;

impl<'a> From<Err<NomError<'a>>> for Error {
    fn from(e: Err<NomError<'a>>) -> Error {
        match e {
            Err::Incomplete(_) => Error::IncompleteInput,
            Err::Failure(e) => Error::ReadError(e.errors.iter().map(|elt| elt.into()).collect()),
            Err::Error(e) => Error::ReadError(e.errors.iter().map(|elt| elt.into()).collect()),
        }
    }
}

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

pub fn parse<T: Source>(source: &mut T) -> std::result::Result<Datum, Error> {
    sexp::parse(source)
}

pub fn parse_sequence<T: Source>(source: &mut T) -> std::result::Result<Vec<Datum>, Error> {
    sexp::parse_sequence(source)
}
