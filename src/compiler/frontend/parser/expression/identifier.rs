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
#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::source::SourceType;

    #[test]
    fn test_identifier_equals() {
        let x = Identifier::new(String::from("foo"), location(0, 1));
        let y = Identifier::new(String::from("foo"), location(10, 1));

        assert_eq!(x, y)
    }

    fn location(line: usize, col: usize) -> SourceLocation {
        SourceLocation::new(
            SourceType::Buffer("datum-parser-test".to_string()),
            line,
            col,
        )
    }
}
