pub mod numeric;
use super::printer::Print;
use crate::vm::byte_code::chunk;
use crate::vm::environment;

#[repr(transparent)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Symbol(pub String);

// Scheme values used at runtime by the VM
// Every scheme expression eventually evaluates to a value of this kind
#[derive(Debug, Clone)]
pub enum Value {
    Symbol(Symbol),
    Callable(Callable),
    Number(Numeric),
}

impl PartialEq for Value {
    fn eq(&self, rhs: &Value) -> bool {
        match (self, rhs) {
            (Value::Symbol(ls), Value::Symbol(rs)) => ls == rs,
            (Value::Callable(_), Value::Callable(_)) => false,
            (Value::Number(ln), Value::Number(rn)) => ln == rn,
            _ => false,
        }
    }
}

impl Eq for Value {}

#[derive(Clone, Debug)]
pub enum Callable {
    Procedure(Procedure),
    BuiltinProcedure(BuiltinProcedure),
}

#[derive(Debug, Clone)]
pub struct Procedure {
    pub arity: u16,
    pub chunk: chunk::Chunk,
}

#[derive(Debug, Clone, Copy)]
pub struct BuiltinProcedure;

#[repr(transparent)]
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Numeric {
    Fixnum(i64),
}

pub fn fixnum(val: i64) -> Value {
    Value::Number(Numeric::Fixnum(val))
}

pub fn sym(val: String) -> Value {
    Value::Symbol(Symbol(val))
}

impl Print for Value {
    fn print(&self) -> Option<String> {
        match self {
            Value::Number(Numeric::Fixnum(num)) => Some(format!("{}", num)),
            _ => None,
        }
    }
}
