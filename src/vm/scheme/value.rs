use std::collections::LinkedList;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Bool(bool),
    Symbol(String),
    ProperList(LinkedList<Box<Value>>),
    Unspecified,
}

impl Value {
    pub fn unspecified() -> Value {
        Self::Unspecified
    }

    pub fn boolean(val: bool) -> Value {
        Self::Bool(val)
    }

    pub fn symbol(name: &str) -> Value {
        Self::Symbol(name.to_string())
    }

    pub fn proper_list<'a, I>(elts: &mut I) -> Value
    where
        I: Iterator<Item = &'a Value>,
    {
        let ls: LinkedList<Box<Value>> = elts.map(|e| Box::new(e.clone())).collect();
        Self::ProperList(ls)
    }
}
