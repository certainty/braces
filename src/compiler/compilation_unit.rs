use crate::vm::value;

#[derive(Clone, Debug)]
pub struct CompilationUnit {
    pub values: value::Factory,
    pub closure: value::closure::Closure,
}

impl CompilationUnit {
    pub fn new(values: value::Factory, closure: value::closure::Closure) -> Self {
        CompilationUnit { values, closure }
    }
}
