use super::writer::ExternalRepresentation;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Bool(bool),
    Unspecified,
}

impl ExternalRepresentation for Value {
    fn external_repr(&self) -> &str {
        match self {
            Value::Bool(true) => "#t",
            Value::Bool(false) => "#f",
            Value::Unspecified => "#<undefined>",
        }
    }
}
