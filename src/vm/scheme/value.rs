pub mod list;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Bool(bool),
    Symbol(String),
    Char(char),
    ProperList(list::List),
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

    pub fn character(c: char) -> Value {
        Self::Char(c)
    }

    pub fn proper_list(vals: Vec<Value>) -> Value {
        let ls: list::List = vals.into();
        Value::ProperList(ls)
    }
}
