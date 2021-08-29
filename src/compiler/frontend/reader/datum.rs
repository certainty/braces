use crate::compiler::frontend::reader::sexp::Sexp;
use crate::compiler::frontend::syntax::symbol::Symbol;
use crate::compiler::source::{HasSourceLocation, Location};
use crate::vm::value::{number, Value};

#[derive(Debug, PartialEq, Clone)]
pub struct Datum {
    location: Location,
    sexp: Sexp,
}

impl Datum {
    pub fn new(sexp: Sexp, location: Location) -> Self {
        Self { sexp, location }
    }

    pub fn sexp(&self) -> &Sexp {
        &self.sexp
    }

    pub fn location(&self) -> &Location {
        &self.location
    }

    pub fn from_value(v: &Value, location: Location) -> Option<Self> {
        match v {
            Value::Char(c) => Some(Self::new(Sexp::character(c.clone()), location)),
            Value::Symbol(sym) => Some(Self::new(Sexp::forged_symbol(sym.as_str()), location)),
            Value::InternedString(s) => Some(Self::new(Sexp::string(s.as_str()), location)),
            Value::UninternedString(s) => Some(Self::new(Sexp::string(s), location)),
            Value::Number(n) => Some(Self::new(Sexp::number(n.clone()), location)),
            Value::Bool(b) => Some(Self::new(Sexp::boolean(b.clone()), location)),
            Value::ProperList(ls) => {
                let elts: Option<Vec<_>> = ls
                    .iter()
                    .map(|e| Self::from_value(e, location.clone()))
                    .collect();

                elts.map(|e| Self::new(Sexp::list(e.iter().cloned()), location))
            }
            _ => None,
        }
    }

    pub fn is_atom(&self) -> bool {
        match self.sexp() {
            Sexp::Vector(_) => false,
            Sexp::List(_) => false,
            Sexp::ImproperList(_, _) => false,
            Sexp::ByteVector(_) => false,
            _ => true,
        }
    }

    pub fn is_symbol(&self) -> bool {
        match self.sexp() {
            Sexp::Symbol(_) => true,
            _ => false,
        }
    }

    pub fn list_slice(&self) -> Option<&[Datum]> {
        match self.sexp() {
            Sexp::List(ls) => Some(&ls[..]),
            _ => None,
        }
    }
}

impl ToString for Datum {
    fn to_string(&self) -> String {
        self.sexp().to_string()
    }
}

impl HasSourceLocation for Datum {
    fn source_location(&self) -> &Location {
        &self.location
    }
}
