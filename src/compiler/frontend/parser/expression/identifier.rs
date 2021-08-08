use super::parse_result::ParseResult;
use super::Expression;
use crate::compiler::frontend::parser::syntax::Symbol;
use crate::compiler::frontend::reader::sexp::datum::{Datum, Sexp};
use crate::compiler::source::SourceType;
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};

#[derive(Clone, Debug)]
pub struct Identifier {
    name: Symbol,
    location: SourceLocation,
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Identifier {
    pub fn new<T: Into<Symbol>>(s: T, location: SourceLocation) -> Self {
        Self {
            name: s.into(),
            location,
        }
    }

    pub fn symbol(&self) -> &Symbol {
        &self.name
    }

    pub fn synthetic(s: &str) -> Identifier {
        Self::new(Symbol::forged(s), SourceType::Synthetic.location(0, 0))
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
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        &self.location
    }
}

pub fn parse(datum: &Datum) -> ParseResult<Expression> {
    parse_identifier(datum).map(Expression::Identifier)
}

pub fn parse_identifier(datum: &Datum) -> ParseResult<Identifier> {
    match datum.sexp() {
        Sexp::Symbol(s) => {
            ParseResult::accept(Identifier::new(s.clone(), datum.source_location().clone()))
        }
        _ => ParseResult::ignore("Expected identifier", datum.source_location().clone()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::expression::tests::*;

    #[test]
    fn test_identifier_equals() {
        let x = Identifier::new(Symbol::forged("foo"), location(0, 1));
        let y = Identifier::new(Symbol::forged("foo"), location(10, 1));

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
