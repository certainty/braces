use super::error::ParseError;
use crate::compiler::source::Source;
use crate::compiler::source_location::SourceLocation;

type Result<T> = std::result::Result<T, ParseError>;

pub enum Datum {
    Boolean(bool, SourceLocation),
}

impl Datum {
    pub fn parse(source: &mut dyn Source) -> Result<Datum> {
        let mut buffer = String::new();
        source.read_to_string(&mut buffer)?;
    }

    pub fn boolean(value: bool, source_location: SourceLocation) -> Datum {
        Self::Boolean(value, source_location)
    }
}
