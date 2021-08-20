use super::Expression;
use super::{ParseResult, Parser};
use crate::compiler::frontend::reader::sexp::datum::{Datum, Sexp};
use crate::compiler::source::{HasSourceLocation, Location, SourceId};

#[derive(Clone, Debug)]
pub struct Identifier {
    name: String,
    location: Location,
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Identifier {
    pub fn new<T: Into<String>>(s: T, location: Location) -> Self {
        Self {
            name: s.into(),
            location,
        }
    }
    pub fn synthetic(s: &str) -> Identifier {
        Self::new(s, Location::new(SourceId::from(0), 0..0))
    }

    pub fn string(&self) -> &String {
        &self.name
    }
}

impl From<Identifier> for String {
    fn from(id: Identifier) -> String {
        id.name
    }
}

impl HasSourceLocation for Identifier {
    fn source_location<'a>(&'a self) -> &'a Location {
        &self.location
    }
}

impl Expression {
    pub fn identifier(str: String, loc: Location) -> Expression {
        Expression::Identifier(Identifier::new(str, loc))
    }
}

impl Parser {
    pub fn parse(&mut self, datum: &Datum) -> ParseResult<Expression> {
        self.parse_identifier(datum).map(Expression::Identifier)
    }

    pub fn do_parse_identifier(&mut self, datum: &Datum) -> ParseResult<Identifier> {
        match datum.sexp() {
            Sexp::Symbol(s) => {
                ParseResult::accept(Identifier::new(s, datum.source_location().clone()))
            }
            _ => ParseResult::ignore("Expected identifier", datum.source_location().clone()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::tests::*;

    #[test]
    fn test_identifier_equals() {
        let x = Identifier::new(String::from("foo"), location(0, 1));
        let y = Identifier::new(String::from("foo"), location(10, 1));

        assert_eq!(x, y)
    }

    #[test]
    fn test_parse_identifier() {
        assert_parse_as(
            "foo",
            Expression::identifier("foo".to_string(), location(0, 0)),
        )
    }
}
