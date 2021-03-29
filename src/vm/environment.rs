use super::value::symbol::InternedSymbol;
use super::value::Value;
use crate::vm::hash_map;

type Bindings = hash_map::HashMap<InternedSymbol, Value>;

#[derive(Debug, Clone)]
pub struct Environment {
    scopes: Vec<Bindings>,
}

impl Environment {
    pub fn empty() -> Self {
        Environment {
            scopes: vec![hash_map::new()],
        }
    }

    pub fn push_scope(&mut self) -> &mut Self {
        self.scopes.push(hash_map::new());
        self
    }

    pub fn pop_scope(&mut self) -> &mut Self {
        self.scopes.pop();
        self
    }

    pub fn get(&self, sym: &InternedSymbol) -> Option<&Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(sym) {
                return Some(value);
            }
        }
        None
    }

    pub fn set(&mut self, sym: &InternedSymbol, val: Value) -> &mut Self {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(sym.clone(), val);
        }
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::value;
    use crate::vm::value::symbol::SymbolTable;

    #[test]
    fn test_get_empty() {
        let symbols = SymbolTable::new();
        let sym = symbols.interned("test".into());
        let env = Environment::empty();

        assert_eq!(env.get(&sym), None)
    }

    #[test]
    fn test_set_then_get() {
        let symbols = SymbolTable::new();
        let sym = symbols.interned("test".into());
        let mut env = Environment::empty();

        assert_eq!(env.get(&sym), None);

        env.set(&sym, value::fixnum(10));
        assert_eq!(env.get(&sym), Some(&value::fixnum(10)));
    }

    #[test]
    fn test_get_scopes() {
        let symbols = SymbolTable::new();
        let sym = symbols.interned("test".into());
        let mut env = Environment::empty();

        env.set(&sym, value::fixnum(10));
        env.push_scope().set(&sym, value::fixnum(12));

        assert_eq!(env.get(&sym), Some(&value::fixnum(12)));

        env.pop_scope();

        assert_eq!(env.get(&sym), Some(&value::fixnum(10)));
    }

    #[test]
    fn test_get_from_outer_scope() {
        let symbols = SymbolTable::new();
        let sym = symbols.interned("test".into());
        let mut env = Environment::empty();

        env.set(&sym, value::fixnum(10));
        env.push_scope().push_scope();

        assert_eq!(env.get(&sym), Some(&value::fixnum(10)));
    }
}
