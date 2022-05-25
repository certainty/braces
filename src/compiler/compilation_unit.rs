use crate::vm::value;

// this will go somewhere else, not sure where
// What is a module?
// A module is a collection of bindings, which allows control over their visibility
// A module is a means to package up functions and values that belong together
// A module creates a namespace for syntactic identifiers

// When a module is loaded it expands the environemnt with its exported bindings
// When a module is loaded its body is evaluated (in which environment though?)
//
// Is a module a runtime or compile time construct? Is it both?
// What about macros defined in modules?

#[derive(Clone, Debug)]
pub struct Module {
    entry_point: value::closure::Closure,
}

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
