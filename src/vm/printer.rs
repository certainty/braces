use crate::vm::value::symbol;
// the scheme printer

pub trait Print {
    fn print(&self, symbols: &symbol::SymbolTable) -> Option<String>;
}

pub fn print<T: Print>(val: &T, symbols: &symbol::SymbolTable) -> String {
    val.print(symbols).unwrap_or(String::from("#<unprintable>"))
}
