use crate::vm::printer::Print;

#[repr(transparent)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Symbol(pub String);

impl Print for Symbol {
    fn print(&self) -> Option<String> {
        Some(self.0.clone())
    }
}
