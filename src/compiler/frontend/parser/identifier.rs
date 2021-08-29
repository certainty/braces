use crate::compiler::frontend::parser::core_parser::CoreParser;
use crate::compiler::frontend::reader::{datum::Datum, sexp::SExpression};
use crate::compiler::frontend::syntax::symbol::Symbol;
use crate::compiler::source::{HasSourceLocation, Location, SourceId};

use super::Expression;
use super::ParseResult;

#[derive(Clone, Debug)]
pub struct Identifier {
    name: Symbol,
    location: Location,
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Identifier {
    pub fn new<T: Into<Symbol>>(s: T, location: Location) -> Self {
        Self {
            name: s.into(),
            location,
        }
    }

    pub fn symbol(&self) -> &Symbol {
        &self.name
    }

    pub fn synthetic(s: &str) -> Identifier {
        Self::new(
            Symbol::forged(s),
            Location::new(SourceId::synthetic(), 0..0),
        )
    }

    pub fn string(&self) -> &String {
        &self.name.string()
    }
}

impl From<Identifier> for String {
    fn from(id: Identifier) -> String {
        id.name.string().clone()
    }
}

impl HasSourceLocation for Identifier {
    fn source_location(&self) -> &Location {
        &self.location
    }
}

impl Expression {
    pub fn identifier(str: String, loc: Location) -> Expression {
        Expression::Identifier(Identifier::new(str, loc))
    }
}

impl CoreParser {
    pub fn parse_identifier(&mut self, datum: &Datum) -> ParseResult<Expression> {
        self.do_parse_identifier(datum).map(Expression::Identifier)
    }

    pub fn do_parse_identifier(&mut self, datum: &Datum) -> ParseResult<Identifier> {
        match datum.s_expression() {
            SExpression::Symbol(s) => {
                ParseResult::accept(Identifier::new(s.clone(), datum.source_location().clone()))
            }
            _ => ParseResult::ignore("Expected identifier", datum.source_location()),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::frontend::parser::tests::*;

    use super::*;

    #[test]
    fn test_identifier_equals() {
        let x = Identifier::new(String::from("foo"), location(0..1));
        let y = Identifier::new(String::from("foo"), location(10..1));

        assert_eq!(x, y)
    }

    #[test]
    fn test_parse_identifier() {
        assert_parse_as(
            "foo",
            Expression::identifier("foo".to_string(), location(0..0)),
        )
    }
}
