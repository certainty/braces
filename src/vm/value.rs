pub mod numeric;
pub mod procedure;
pub mod symbol;

use super::printer::Print;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Symbol(symbol::Symbol),
    Procedure(procedure::Procedure),
    Number(numeric::Number),
}

pub fn fixnum(val: i64) -> Value {
    Value::Number(numeric::Number::Fixnum(val))
}

pub fn sym(val: String) -> Value {
    Value::Symbol(symbol::Symbol(val))
}

impl Print for Value {
    fn print(&self) -> Option<String> {
        match self {
            Value::Number(num) => num.print(),
            Value::Symbol(sym) => sym.print(),
            _ => None,
        }
    }
}
