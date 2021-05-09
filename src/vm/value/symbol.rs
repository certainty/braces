use super::equality::SchemeEqual;
use crate::compiler::utils::string_table;

#[repr(transparent)]
#[derive(Clone, PartialEq, Hash, Eq)]
pub struct Symbol(pub string_table::Interned);

impl Symbol {
    pub fn as_str<'a>(&'a self) -> &'a str {
        self.0.as_str()
    }
}

impl std::fmt::Debug for Symbol {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
    ) -> std::result::Result<(), std::fmt::Error> {
        formatter.write_fmt(format_args!("sym#({})", self.as_str()))
    }
}

impl SchemeEqual<Symbol> for Symbol {
    fn is_eq(&self, other: &Symbol) -> bool {
        self.0 == other.0
    }

    fn is_eqv(&self, other: &Symbol) -> bool {
        self.0 == other.0
    }

    fn is_equal(&self, other: &Symbol) -> bool {
        self.0 == other.0
    }
}
