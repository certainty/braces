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
use expression::Expression;
use syntax::SelfEvaluating;
use syntax::Syntax::*;
use thiserror::Error;

#[derive(PartialEq, Debug, Clone)]
pub struct Location {
    line: usize,
}

#[derive(PartialEq, Debug)]
pub struct SourceInformation {
    location: Location,
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("ParseError")]
    ParseError(String, SourceInformation),
    #[error(transparent)]
    ReaderError(#[from] reader::ReaderError),
}

type Result<T> = std::result::Result<T, ParseError>;

pub fn parse<T: source::Source>(source: &mut T) -> Result<Option<expression::Expression>> {
    let ast = reader::read_datum(source)?;

    match ast {
        Some(datum) => parse_single(&datum).map(Some),
        None => Ok(None),
    }
}

// Parse syntax into scheme expressions
fn parse_single(datum: &syntax::Syntax) -> Result<expression::Expression> {
    match datum {
        SelfEvaluatingSyntax(SelfEvaluating::Symbol(sym), loc) => {
            Ok(expression::variable(sym, source_info(loc)))
        }
        SelfEvaluatingSyntax(syn, loc) => Ok(expression::literal(syn.clone(), source_info(loc))),
        ProperList(elements, location) => match elements.first() {
            Some(SelfEvaluatingSyntax(SelfEvaluating::Symbol(op), _)) => match op.as_str() {
                "if" => Ok(parse_if(&elements, location)?),
                _ => Err(parse_error("Unsupported syntax", source_info(location))),
            },
            _ => Err(parse_error(
                "Invalid empty list expression",
                source_info(location),
            )),
        },
        _ => panic!("Unsupported syntax"),
    }
}

fn parse_error(message: &str, source: SourceInformation) -> ParseError {
    ParseError::ParseError(message.into(), source)
}

fn parse_if(elements: &Vec<syntax::Syntax>, location: &Location) -> Result<expression::Expression> {
    match &elements[..] {
        [_, test, consequent] => Ok(Expression::If(
            Box::new(parse_single(&test)?),
            Box::new(parse_single(&consequent)?),
            None,
            source_info(&location),
        )),

        [_, test, consequent, alternate] => Ok(Expression::If(
            Box::new(parse_single(&test)?),
            Box::new(parse_single(&consequent)?),
            Some(Box::new(parse_single(&alternate)?)),
            source_info(&location),
        )),
        _ => Err(parse_error("Invalid if expression", source_info(&location))),
    }
}

fn source_info(location: &Location) -> SourceInformation {
    SourceInformation {
        location: location.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::expression::Expression;
    use super::*;

    fn make_source_info(line: usize) -> SourceInformation {
        source_info(&Location { line })
    }

    #[test]
    fn test_literal_number() {
        assert_matches!(
            run_parser("23545").unwrap(),
            Some(Expression::Literal(_, _))
        )
    }

    fn run_parser(inp: &str) -> Result<Option<expression::Expression>> {
        let mut source: source::StringSource = inp.into();

        parse(&mut source)
    }
}
