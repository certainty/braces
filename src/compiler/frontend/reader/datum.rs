use crate::compiler::source_location::SourceLocation;
use crate::vm::scheme::value::Value;

#[derive(Debug, PartialEq)]
pub struct Datum {
    pub location: SourceLocation,
    pub value: Value,
}

impl Datum {
    pub fn new(value: Value, location: SourceLocation) -> Self {
        Self { value, location }
    }

    pub fn value(&self) -> &Value {
        &self.value
    }

    pub fn location(&self) -> &SourceLocation {
        &self.location
    }
}
