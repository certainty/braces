use super::value::symbol::Symbol;
use super::value::Value;
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

    pub fn binding_names(&self) -> Vec<String> {
        self.bindings
            .keys()
            .map(|k| String::from(k.as_str()))
            .collect()
    }

    pub fn set(&mut self, k: Symbol, v: Value) {
        self.bindings.insert(k, v);
    }

    pub fn get(&self, k: &Symbol) -> Option<&Value> {
        self.bindings.get(k)
    }

    pub fn get_owned(&self, k: &Symbol) -> Option<Value> {
        self.bindings.get(k).map(|e| e.clone())
    }
}
