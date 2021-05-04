use super::error::Error;
use super::Expression;
use super::Result;
use crate::compiler::frontend::parser::sexp::datum::Datum;
use crate::compiler::source::SourceType;
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};

#[derive(Clone, Debug)]
pub struct Identifier {
    name: String,
    location: SourceLocation,
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Identifier {
    pub fn new<T: Into<String>>(s: T, location: SourceLocation) -> Self {
        Self {
            name: s.into(),
            location,
        }
    }
    pub fn synthetic(s: &str) -> Identifier {
        Self::new(s, SourceType::Synthetic.location(0, 0))
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
    fn source_location<'a>(&'a self) -> &'a SourceLocation {
        &self.location
    }
}

/// Parses the datum as an identifier and fails if it's not a valid identifier
pub fn parse(datum: &Datum) -> Result<Identifier> {
    let id_expr = Expression::parse_expression(datum)?;
    if let Expression::Identifier(id) = id_expr {
        Ok(id)
    } else {
        Error::parse_error("Expected identifier", datum.location.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::expression::tests::*;

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
