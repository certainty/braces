use super::value;
use std::collections::HashMap;

pub struct Environment {
    bindings: Vec<HashMap<value::Symbol, value::Value>>,
}

impl Environment {
    pub fn empty() -> Environment {
        Environment {
            bindings: vec![HashMap::new()],
        }
    }

    pub fn extend(&mut self) -> &mut Self {
        self.bindings.push(HashMap::new());
        self
    }

    pub fn reduce(&mut self) -> &mut Self {
        self.bindings.pop();
        self
    }

    pub fn get(&self, sym: &value::Symbol) -> Option<&value::Value> {
        for env in self.bindings.iter().rev() {
            match env.get(sym) {
                Some(found) => {
                    return Some(found);
                }
                _ => (),
            }
        }
        None
    }

    pub fn set(&mut self, sym: &value::Symbol, val: value::Value) {
        match self.bindings.last_mut() {
            Some(b) => {
                b.insert(sym.clone(), val);
                ()
            }
            _ => (),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::value;
    use super::*;

    #[test]
    fn test_get_empty() {
        let sym = value::Symbol("test".into());
        let env = Environment::empty();

        assert_eq!(env.get(&sym), None)
    }

    #[test]
    fn test_set_then_get() {
        let sym = value::Symbol("test".into());
        let mut env = Environment::empty();

        assert_eq!(env.get(&sym), None);

        env.set(&sym, value::fixnum(10));
        assert_eq!(env.get(&sym), Some(&value::fixnum(10)));
    }

    #[test]
    fn test_get_scopes() {
        let sym = value::Symbol("test".into());
        let mut env = Environment::empty();

        env.set(&sym, value::fixnum(10));
        env.extend().set(&sym, value::fixnum(12));

        assert_eq!(env.get(&sym), Some(&value::fixnum(12)));

        env.reduce();

        assert_eq!(env.get(&sym), Some(&value::fixnum(10)));
    }

    #[test]
    fn test_get_from_outer_scope() {
        let sym = value::Symbol("test".into());
        let mut env = Environment::empty();

        env.set(&sym, value::fixnum(10));
        env.extend().extend();

        assert_eq!(env.get(&sym), Some(&value::fixnum(10)));
    }
}
