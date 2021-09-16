use super::value::symbol::Symbol;
use super::value::Value;
use crate::vm::place::Reference;
use rustc_hash::FxHashMap;

#[derive(Debug)]
pub struct TopLevel {
    bindings: FxHashMap<Symbol, Reference<Value>>,
}

impl TopLevel {
    pub fn new() -> Self {
        Self {
            bindings: FxHashMap::default(),
        }
    }

    pub fn binding_names(&self) -> Vec<String> {
        self.bindings
            .keys()
            .map(|k| String::from(k.as_str()))
            .collect()
    }

    pub fn define(&mut self, k: Symbol, v: Value) {
        self.bindings.insert(k, v.into());
    }

    pub fn set(&mut self, k: Symbol, v: Value) {
        self.bindings.get_mut(&k).map(|r| r.set(v));
    }

    pub fn get(&self, k: &Symbol) -> Option<&Reference<Value>> {
        self.bindings.get(k)
    }

    pub fn get_owned(&self, k: &Symbol) -> Option<Value> {
        self.bindings.get(k).map(|e| e.get_inner())
    }
}
