use crate::compiler::frontend::error;
use crate::compiler::frontend::reader::sexp::Input;
use crate::compiler::source::Location;
use nom::error::{Error, ErrorKind};
use nom::Err;

type NomError<'a> = Error<Input<'a>>;

impl<'a> From<(Input<'a>, ErrorKind)> for error::Detail {
    fn from(e: (Input<'a>, ErrorKind)) -> error::Detail {
        let (input, _kind) = e;
        let content = input.fragment();
        let span = input.location_offset()..input.fragment().len();
        let location = Location::new(input.extra.clone(), span);
        error::Detail::new(content.to_string(), location)
    }
}

impl<'a> From<Err<NomError<'a>>> for error::Error {
    fn from(e: Err<NomError<'a>>) -> error::Error {
        match e {
            Err::Incomplete(_) => {
                error::Error::incomplete_input("incomplete input while reading source file", None)
            }
            Err::Failure(e) => error::Error::read_error(
                "Fatal error while reading input",
                (e.input, e.code).into(),
                vec![],
            ),
            Err::Error(e) => error::Error::read_error(
                "Error while reading input",
                (e.input, e.code).into(),
                vec![],
            ),
        }
    }
}
