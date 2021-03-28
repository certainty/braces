use crate::vm::hash_map;
use crate::vm::value::symbol::Symbol;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    symbols: hash_map::HashMap<u32, String>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            symbols: hash_map::new(),
        }
    }

    pub fn intern(&mut self, value: String) -> Symbol {
        let hsh = hash_map::hash(value.as_str());
        self.symbols.insert(hsh, value);
        Symbol(hsh)
    }

    pub fn name(&self, sym: &Symbol) -> Option<&String> {
        self.symbols.get(&sym.0)
    }
}
