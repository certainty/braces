use crate::compiler::utils::string_table;
use crate::vm::scheme::equality::SchemeEqual;

#[repr(transparent)]
#[derive(Clone, PartialEq)]
pub struct InternedString(pub string_table::Interned);

impl InternedString {
    pub fn as_str<'a>(&'a self) -> &'a str {
        self.0.as_str()
    }
}

impl std::fmt::Debug for InternedString {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
    ) -> std::result::Result<(), std::fmt::Error> {
        formatter.write_fmt(format_args!(
            "Interned({} @ {:p})",
            self.as_str(),
            self.as_str()
        ))
    }
}

impl SchemeEqual<InternedString> for InternedString {
    fn is_eq(&self, other: &InternedString) -> bool {
        self.0.is_identifical(&other.0)
    }

    fn is_eqv(&self, other: &InternedString) -> bool {
        self.0.is_identifical(&other.0)
    }

    fn is_equal(&self, other: &InternedString) -> bool {
        self.as_str() == other.as_str()
    }
}
