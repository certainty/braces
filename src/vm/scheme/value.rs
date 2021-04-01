#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Bool(bool),
    Symbol(String),
    Unspecified,
}
