use crate::vm::hash_map;
use crate::vm::printer::Print;

#[repr(transparent)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Symbol(pub u64);

impl Symbol {
    pub fn intern(s: &str) -> Symbol {
        Symbol(hash_map::hash(s))
    }
}

impl Print for Symbol {
    fn print(&self) -> Option<String> {
        // TODO: use symbol table to retrieve name
        Some("#<symbol>".into())
    }
}
