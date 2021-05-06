use super::procedure;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub proc: Rc<procedure::Procedure>,
}
