/// The scheme parser that turns read syntax into expressions
///
/// The schema parser, similar to the runtime, relies on the scheme reader
/// to turn an input stream into a sequence of syntax. This parser takes the syntax
/// and parses it into expressions.
///
/// As scheme supports quoted data, the parser has to conditionally disable "evaluation"
/// which is straight forward todo. This is one reason why we re-use the underlying low
/// level parser in the reader.
pub mod expression;
mod lowlevel;
pub mod reader;
pub mod source;
mod syntax;
use syntax::SelfEvaluating;
use syntax::Syntax::*;
use thiserror::Error;

#[derive(PartialEq, Debug)]
pub struct Location {
    line: usize,
}

#[derive(PartialEq, Debug)]
pub struct SourceInformation {
    location: Location,
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error(transparent)]
    ReaderError(#[from] reader::ReaderError),
}

type Result<T> = std::result::Result<T, ParseError>;

pub fn parse<T: source::Source>(source: &mut T) -> Result<Option<expression::Expression>> {
    let ast = reader::read_datum(source)?;

    match ast {
        Some(datum) => parse_single(datum).map(Some),
        None => Ok(None),
    }
}

/// Convert syntax into expressions
fn parse_single(datum: syntax::Syntax) -> Result<expression::Expression> {
    match datum {
        SelfEvaluatingSyntax(SelfEvaluating::Symbol(sym), loc) => Ok(expression::variable(
            expression::symbol(sym),
            source_info(loc),
        )),
        SelfEvaluatingSyntax(syn, loc) => Ok(expression::literal(syn, source_info(loc))),
        ProperList(elements, location) => match elements.first() {
            Some(SelffEvaluating::Symbol("if")) => Ok(parseIf(elements, location)?),
        },
        _ => panic!("Unsupported syntax"),
    }
}

fn parseIf(Vec<syntax::Syntax>, location: Location) -> Result<expression::Expression> {

}

fn source_info(location: Location) -> SourceInformation {
    SourceInformation { location }
}

#[cfg(test)]
mod tests {
    use super::expression::Expression;
    use super::*;

    #[test]
    fn test_literal_number() {
        assert_matches!(run_parser("23545").unwrap(), Some(Expression::Literal(_)))
    }

    fn run_parser(inp: &str) -> Result<Option<expression::Expression>> {
        let mut source: source::StringSource = inp.into();

        parse(&mut source)
    }
}
