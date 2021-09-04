use crate::vm::value;

#[derive(Clone, Debug)]
pub struct CompilationUnit {
    // The closure to execute this compilation unit
    pub closure: value::closure::Closure,
}

impl CompilationUnit {
    pub fn new(closure: value::closure::Closure) -> Self {
        CompilationUnit { closure }
    }
}
