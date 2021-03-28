use crate::vm::printer::Print;
use crate::vm::symbol_table;

#[repr(transparent)]
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Number {
    Fixnum(i64),
}

impl Print for Number {
    fn print(&self, _: &symbol_table::SymbolTable) -> Option<String> {
        match self {
            Number::Fixnum(num) => Some(format!("{}", &num)),
        }
    }
}
