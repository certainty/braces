use super::equality::SchemeEqual;
use std::rc::Rc;

#[repr(transparent)]
#[derive(Clone, Debug, PartialEq)]
pub struct String(Rc<std::string::String>);

impl String {
    pub fn new() -> Self {
        Self(Rc::new(std::string::String::from("")))
    }
}

impl AsRef<str> for String {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl From<&str> for String {
    fn from(other: &str) -> Self {
        Self(Rc::from(std::string::String::from(other)))
    }
}

impl From<std::string::String> for String {
    fn from(s: std::string::String) -> Self {
        Self(Rc::from(s))
    }
}

impl From<Rc<std::string::String>> for String {
    fn from(s: Rc<std::string::String>) -> Self {
        Self(s)
    }
}

impl From<&Rc<std::string::String>> for String {
    fn from(s: &Rc<std::string::String>) -> Self {
        Self(s.clone())
    }
}

impl SchemeEqual<String> for String {
    fn is_eq(&self, other: &String) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    fn is_eqv(&self, other: &String) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    fn is_equal(&self, other: &String) -> bool {
        self.0.as_ref() == other.0.as_ref()
    }
}
