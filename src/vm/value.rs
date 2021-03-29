pub mod numeric;
pub mod procedure;
pub mod symbol;

use super::printer::Print;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Symbol(symbol::Symbol),
    Procedure(procedure::Lambda),
    ForeignProcedure(procedure::ForeignLambda),
    Number(numeric::Number),
}

impl Print for Value {
    fn print(&self, symbols: &symbol::SymbolTable) -> Option<String> {
        match self {
            Value::Number(num) => num.print(symbols),
            Value::Symbol(sym) => sym.print(symbols),
            Value::ForeignProcedure(proc) => proc.print(symbols),
            Value::Procedure(proc) => proc.print(symbols),
            _ => None,
        }
    }
}

pub fn fixnum(val: i64) -> Value {
    Value::Number(numeric::Number::Fixnum(val))
}

pub fn foreign_lambda(arity: procedure::Arity, operation: procedure::ForeignLambdaFunc) -> Value {
    Value::ForeignProcedure(procedure::ForeignLambda { arity, operation })
}
