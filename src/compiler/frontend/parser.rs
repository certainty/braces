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
mod syntax;

use crate::compiler::source;
use crate::compiler::source::{Location, SourceInformation};
use crate::vm::value;
use expression::Expression;
use syntax::SelfEvaluating;
use syntax::Syntax::*;
use thiserror::Error;

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

        SelfEvaluatingSyntax(syn, loc) => parse_literal(syn, loc),

        ProperList(elements, location) => match elements.first() {
            Some(SelfEvaluatingSyntax(SelfEvaluating::Symbol(op), _)) => match op.as_str() {
                "if" => parse_if(&elements, location),
                _ => parse_application(elements, location),
            },
            _ => parse_application(&elements, location),
        },
        _ => panic!("Unsupported syntax"),
    }
}

fn parse_error(message: &str, source: SourceInformation) -> ParseError {
    ParseError::ParseError(message.into(), source)
}

fn parse_literal(syn: &SelfEvaluating, location: &Location) -> Result<expression::Expression> {
    let value = match syn {
        &SelfEvaluating::FixNum(num) => value::fixnum(num),
        _ => panic!("Unsupported value"), //TODO: remove once we support everything
    };

    Ok(expression::literal(value, source_info(location)))
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

fn parse_application(
    elements: &Vec<syntax::Syntax>,
    location: &Location,
) -> Result<expression::Expression> {
    match &elements[..] {
        [operator, operands @ ..] => {
            let parsed_operator = parse_single(&operator)?;
            let parsed_operands: Vec<Expression> = operands
                .iter()
                .map(parse_single)
                .collect::<Result<Vec<Expression>>>()?;

            Ok(Expression::Application(
                Box::new(parsed_operator),
                parsed_operands,
                source_info(&location),
            ))
        }
        _ => Err(parse_error("Invalid application", source_info(&location))),
    }
}

fn source_info(location: &Location) -> SourceInformation {
    SourceInformation::new(location.clone())
}

#[cfg(test)]
mod tests {
    use super::expression::Expression;
    use super::*;

    fn make_source_info(line: usize) -> SourceInformation {
        source_info(&Location::new(line))
    }

    #[test]
    fn test_literal_number() {
        assert_matches!(
            run_parser("23545").unwrap(),
            Some(Expression::Literal(_, _))
        )
    }

    #[test]
    fn test_parse_application_simple() {
        assert_eq!(
            run_parser("(+ 1 2)").unwrap(),
            Some(expression::application(
                expression::variable("+", make_source_info(1)),
                vec![
                    expression::literal(value::fixnum(1), make_source_info(1)),
                    expression::literal(value::fixnum(2), make_source_info(1))
                ],
                make_source_info(1)
            ))
        )
    }

    fn run_parser(inp: &str) -> Result<Option<expression::Expression>> {
        let mut source: source::StringSource = inp.into();

        parse(&mut source)
    }
}
