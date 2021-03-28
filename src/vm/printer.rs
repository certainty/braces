use crate::vm::symbol_table;
// the scheme printer

pub trait Print {
    fn print(&self, symbols: &symbol_table::SymbolTable) -> Option<String>;
}

pub fn print<T: Print>(val: &T, symbols: &symbol_table::SymbolTable) -> String {
    val.print(symbols).unwrap_or(String::from("#<unprintable>"))
}
