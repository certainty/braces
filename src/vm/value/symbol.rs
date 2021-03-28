use crate::vm::hash_map;
use crate::vm::printer::Print;
use crate::vm::symbol_table;

#[repr(transparent)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Symbol(pub u32);

impl Symbol {
    pub fn intern(s: &str) -> Symbol {
        Symbol(hash_map::hash(s))
    }
}

impl Print for Symbol {
    fn print(&self, symbols: &symbol_table::SymbolTable) -> Option<String> {
        if let Some(name) = symbols.name(&self) {
            Some(name.clone())
        } else {
            Some("#<symbol>".into())
        }
    }
}
