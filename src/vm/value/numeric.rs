use crate::vm::printer::Print;

#[repr(transparent)]
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Number {
    Fixnum(i64),
}

impl Print for Number {
    fn print(&self) -> Option<String> {
        match self {
            Number::Fixnum(num) => Some(format!("{}", &num)),
        }
    }
}
