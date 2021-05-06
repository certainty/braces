use super::procedure;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub proc: Rc<procedure::Procedure>,
}

impl From<Rc<procedure::Procedure>> for Closure {
    fn from(v: Rc<procedure::Procedure>) -> Self {
        Closure { proc: v.clone() }
    }
}

impl From<procedure::Procedure> for Closure {
    fn from(v: procedure::Procedure) -> Self {
        Closure { proc: Rc::new(v) }
    }
}
