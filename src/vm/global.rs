use super::value::{Symbol, Value};
use rustc_hash::FxHashMap;

#[derive(Debug)]
pub struct TopLevel {
    bindings: FxHashMap<Symbol, Value>,
}

impl TopLevel {
    pub fn new() -> Self {
        Self {
            bindings: FxHashMap::default(),
        }
    }

    pub fn set(&mut self, k: Symbol, v: Value) {
        self.bindings.insert(k, v);
    }

    pub fn get(&self, k: &Symbol) -> Option<&Value> {
        self.bindings.get(k)
    }
}
