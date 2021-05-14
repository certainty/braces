////////////////////////////////////////////////////////////////////////////////////////////////////
// Locals and local tracking
//
// Variables and variables tracking during the compilation phases
//
// In order to make locals efficient and fast, they're represented as values on the stack.
// This way access to locals is an indexed access into the VM's stack.
// In order to make sure that the address calculation works correctly the code-generator
// tracks (emulates) the state of the stack. Each new scope is introduced by a binding construct
// which means there will be a new stack frame pushed.
// TODO: add better documentation on how this works
//
////////////////////////////////////////////////////////////////////////////////////////////////////

pub mod locals;
pub mod up_values;

use crate::compiler::backend::code_generator::Result;
use crate::compiler::backend::variables::up_values::UpValue;
use crate::compiler::frontend::parser::expression::identifier::Identifier;
use locals::Locals;
use std::{cell::RefCell, rc::Rc};
use up_values::UpValues;

const MAX_LOCALS: usize = 256;
const MAX_UP_VALUES: usize = MAX_LOCALS * 256;

//pub type VariablesRef = Rc<RefCell<Variables>>;

#[repr(transparent)]
#[derive(Debug, Clone, PartialEq)]
pub struct VariablesRef {
    inner: Rc<RefCell<Variables>>,
}

#[derive(Debug, PartialEq)]
pub struct Variables {
    pub parent: Option<VariablesRef>,
    pub locals: Locals,
    pub up_values: UpValues,
    pub scope_depth: usize,
}

impl Variables {
    pub fn child(parent: Option<VariablesRef>) -> VariablesRef {
        VariablesRef {
            inner: Rc::new(RefCell::new(Variables {
                scope_depth: 0,
                locals: Locals::new(MAX_LOCALS),
                up_values: UpValues::new(MAX_UP_VALUES),
                parent,
            })),
        }
    }

    pub fn root() -> VariablesRef {
        Self::child(None)
    }
}

impl VariablesRef {
    pub fn is_root(&self) -> bool {
        self.inner.borrow().parent.is_none()
    }

    pub fn is_top_level(&self) -> bool {
        self.inner.borrow().scope_depth == 0
    }

    pub fn begin_scope(&mut self) {
        self.inner.borrow_mut().scope_depth += 1;
    }

    pub fn up_value_count(&self) -> usize {
        self.inner.borrow().up_values.len()
    }

    pub fn up_values_vec(&self) -> Vec<UpValue> {
        self.inner.borrow().up_values.to_vec()
    }

    // Close the current scope and return information about the discarded variables
    // Returns a Vec<bool> where each true value indicates that a captured variable has been popped
    // and a false value means that the variable was not captured
    pub fn end_scope(&mut self) -> Result<Vec<(usize, bool)>> {
        self.inner.borrow_mut().scope_depth -= 1;

        let mut locals_len = self.inner.borrow().locals.len();
        let mut current_depth = self.inner.borrow().locals.at(locals_len - 1).depth;
        let mut processed_variables: Vec<(usize, bool)> = vec![];

        while locals_len > 0 && current_depth > self.inner.borrow().scope_depth {
            let local = self.inner.borrow_mut().locals.pop()?.unwrap();
            processed_variables.push((locals_len - 1, local.is_captured));

            locals_len -= 1;
            current_depth = self.inner.borrow().locals.at(locals_len - 1).depth;
        }

        processed_variables.reverse();
        Ok(processed_variables)
    }

    pub fn add_up_value(&mut self, local_addr: usize, is_local: bool) -> Result<usize> {
        self.inner.borrow_mut().up_values.add(local_addr, is_local)
    }

    pub fn resolve_up_value(&mut self, name: &Identifier) -> Result<Option<usize>> {
        if self.is_root() {
            return Ok(None);
        }
        let addr = self.resolve_local(name);

        if let Some(local_addr) = addr {
            self.inner.borrow_mut().locals.mark_as_captured(local_addr);
            Ok(Some(self.add_up_value(local_addr, true)?))
        } else {
            let mut parent = self.inner.borrow().parent.as_ref().unwrap().clone();
            let found_up_value_addr = parent.resolve_up_value(name)?;

            if let Some(upvalue_addr) = found_up_value_addr {
                self.add_up_value(upvalue_addr, false).map(Some)
            } else {
                Ok(None)
            }
        }
    }

    #[inline]
    pub fn add_local(&mut self, name: Identifier) -> Result<usize> {
        let scope_depth = self.inner.borrow().scope_depth;
        self.inner.borrow_mut().locals.add(name, scope_depth)?;
        Ok(self.inner.borrow().locals.last_address())
    }

    #[inline]
    pub fn resolve_local(&self, name: &Identifier) -> Option<usize> {
        self.inner.borrow().locals.resolve(name)
    }

    #[inline]
    pub fn mark_local_as_captured(&mut self, address: usize) -> Result<()> {
        Ok(self.inner.borrow_mut().locals.mark_as_captured(address))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolve_local_finds_local_in_current() {
        let mut root = Variables::root();
        root.add_local(Identifier::synthetic("root_x")).unwrap();
        let mut child = Variables::child(Some(root));

        child.add_local(Identifier::synthetic("child_x")).unwrap();

        let local_address = child.resolve_local(&Identifier::synthetic("child_x"));

        assert_eq!(local_address, Some(1));
    }

    #[test]
    fn test_resolve_local_finds_local_in_ancestor() {}

    #[test]
    fn test_variabels_end_scope_identifies_captured_variables() {
        let mut vars = Variables::root();

        vars.begin_scope();

        vars.add_local(Identifier::synthetic("foo")).unwrap();

        let addr = vars.add_local(Identifier::synthetic("bar")).unwrap();

        vars.mark_local_as_captured(addr).unwrap();

        vars.add_local(Identifier::synthetic("baz")).unwrap();

        vars.add_local(Identifier::synthetic("baxz")).unwrap();

        let processed = vars.end_scope().unwrap();

        assert_eq!(
            processed,
            vec![(1, false), (2, true), (3, false), (4, false)]
        )
    }
}
