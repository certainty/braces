use crate::vm::printer::Print;
use crate::vm::value::symbol;

#[repr(transparent)]
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Number {
    Fixnum(i64),
}

impl Print for Number {
    fn print(&self, _: &symbol::SymbolTable) -> Option<String> {
        match self {
            Number::Fixnum(num) => Some(format!("{}", &num)),
        }
    }
}
