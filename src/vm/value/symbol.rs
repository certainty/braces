use crate::vm::hash_map;
use crate::vm::printer::Print;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Symbol {
    Interned(InternedSymbol),
    Uninterned(UninternedSymbol),
}

#[repr(transparent)]
#[derive(Debug, Hash, Copy, Clone, PartialEq, Eq)]
pub struct InternedSymbol(pub(crate) u32);

#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UninternedSymbol(String);

/// The only way to retrieve a symbol is via a symbol table
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

    pub(crate) fn uninterned(&mut self, value: String) -> UninternedSymbol {
        UninternedSymbol(value)
    }

    pub(crate) fn interned(&mut self, value: String) -> InternedSymbol {
        let hsh = hash_map::hash(value.as_str());
        self.symbols.insert(hsh, value);
        InternedSymbol(hsh)
    }

    pub(crate) fn get(&self, sym: &InternedSymbol) -> Option<&String> {
        self.symbols.get(&sym.0)
    }
}

impl From<InternedSymbol> for u32 {
    fn from(sym: InternedSymbol) -> Self {
        sym.0
    }
}

impl Print for Symbol {
    fn print(&self, symbols: &SymbolTable) -> Option<String> {
        match self {
            Symbol::Interned(interned) => interned.print(&symbols),
            Symbol::Uninterned(uninterned) => uninterned.print(&symbols),
        }
    }
}

impl Print for InternedSymbol {
    fn print(&self, symbols: &SymbolTable) -> Option<String> {
        if let Some(name) = symbols.get(self) {
            Some(format!("{}", &name))
        } else {
            Some(format!("#<symbol-anonymous {}>", self.0))
        }
    }
}
impl Print for UninternedSymbol {
    fn print(&self, symbols: &SymbolTable) -> Option<String> {
        Some(format!("#<symbol-uninterned {}>", self.0))
    }
}
