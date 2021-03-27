pub mod numeric;
use super::printer::Print;

// Scheme values used at runtime by the VM
// Every scheme expression eventually evaluates to a value of this kind
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Value {
    Symbol(&'static String), // reference to interned string in symbol table
    Number(Numeric),
}

#[repr(transparent)]
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Numeric {
    Fixnum(i64),
}

pub fn fixnum(val: i64) -> Value {
    Value::Number(Numeric::Fixnum(val))
}

pub fn sym(val: &'static String) -> Value {
    Value::Symbol(val)
}

impl Print for Value {
    fn print(&self) -> Option<String> {
        match self {
            Value::Number(Numeric::Fixnum(num)) => Some(format!("{}", num)),
            _ => None,
        }
    }
}
