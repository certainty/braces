////////////////////////////////////////////////////////////////////////////////////////////////////
// Variables and variables tracking during the compilation phases
//
// Variables and the way they're represented on the VM are an intricate part of the VM and compiler design.
// This module provides helpers to track and address variables which are used in different
// phases of the compiler.
//
// ## Variable scopes
//
// The VM deals with three different scopes, each of which have their characteristics:
//
// 1. global
// 2. closure scope
// 3. local
//
// ### Global
//
// The global scope is the home for all top-level bindings. This is where named procedures
// are registered and module provide their exported bindings. The global scope is generally
// handled by a look-up table, which has a relatively simple runtime representation in the VM.
// From the compiler's point of view there is not much tracking work required. The only
// thing that is required is the knowledge whether of not a variable is global to emit the
// correct byte-code to access those.
//
//
// ### Closure scope
//
// Closure scope is somewhere between local and global. A closure can capture the variables
// of it's surrounding lexical scopes. That means access to closure variables has to be tracked
// carefully to make sure closure variables are hoisted to the heap at appropriate times during
// run-time. The compiler however already has to compute the location of the closure variables (up-values)
// during compile time, which results in a fast index-lookup for up-values at run-time
//
//
// ### Local
//
// Local variables are local to the current scope. Of course since scheme is lexically scoped,
// variables in outer scopes are visible in inner scopes. The VM design that we chose represents
// locals as values on the stack. This means the compiler needs to compute the stack index,
// for each local that is referenced. This work is a bit more complicated for the compiler,
// but allows very efficient access for locals during run-time
//
//
// ## Variables
//
// The main access point to variable handling are the `Variables` and `VariablesRef` types.
// The scopes and their relations are implemented using a linked structure of variable records.
//
// The following depicts the state of two child scopes as represented by the variables manager.
//
//
//           ┌───┬───────────┐
//           │   │ TopLevel  │
//           ├───┴───────────┤
//           │Locals         │
//           ├───────────────┤
//           │Up-Values      │
//           ├───────────────┤
//    ┌─────►│               │
//    │      └───────────────┘
//    │
//    │
//    │
//    │       ┌───┬───────────┐
//    └───────┤   │ Scope 1   │
//            ├───┴───────────┤
//            │Locals         │
//            ├───────────────┤
//            │Up-Values      │
//            ├───────────────┤
//    ┌──────►│               │
//    │       └───────────────┘
//    │
//    │
//    │
//    │       ┌───┬───────────┐
//    └───────┤   │ Scope 2   │
//            ├───┴───────────┤
//            │Locals         │
//            ├───────────────┤
//            │Up-Values      │
//            ├───────────────┤
//            │               │
//            └───────────────┘
//
//
//
// See the `CodeGenerator` on how the variables manager is used.
//
////////////////////////////////////////////////////////////////////////////////////////////////////

pub mod locals;
pub mod up_values;

use crate::compiler::backend::error::Error;
use crate::compiler::backend::variables::up_values::UpValue;
use crate::compiler::backend::Result;
use crate::compiler::frontend::parser::identifier::Identifier;
use locals::Locals;
use std::{cell::RefCell, rc::Rc};
use up_values::UpValues;

const MAX_LOCALS: usize = 256;
const MAX_UP_VALUES: usize = MAX_LOCALS * 256;

// A variables reference that allows interior mutability
// to form a linked list of variable records
#[repr(transparent)]
#[derive(Debug, Clone, PartialEq)]
pub struct VariablesRef {
    inner: Rc<RefCell<Variables>>,
}

// The variables record tracks
// the scope it's associated with and the locals and up-values it knows of
#[derive(Debug, PartialEq)]
pub struct Variables {
    pub parent: Option<VariablesRef>,
    pub locals: Locals,
    pub up_values: UpValues,
    pub scope_depth: usize,
}

impl Variables {
    /// Create a variables record that is the child of the `parent` scope.
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

    // Create a the variables record for the top-level.
    //
    // It will have no parent and thus form the root of the linked structure.
    pub fn top_level() -> VariablesRef {
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

    // Returns a vector of all the up-valus we know of
    pub fn up_values_vec(&self) -> Vec<UpValue> {
        self.inner.borrow().up_values.to_vec()
    }

    // Close the current scope and return information about the discarded variables
    // Returns a Vec<bool> where each true value indicates that a captured variable has been popped
    // and a false value means that the variable was not captured
    pub fn end_scope(&mut self) -> Result<Vec<(usize, bool)>> {
        self.inner.borrow_mut().scope_depth -= 1;

        let mut locals_len = self.inner.borrow().locals.len();

        if locals_len <= 0 {
            return Ok(vec![]);
        }

        let mut current_depth = self.inner.borrow().locals.at(locals_len - 1).depth;
        let mut processed_variables: Vec<(usize, bool)> = vec![];

        while current_depth > self.inner.borrow().scope_depth {
            let local = self.inner.borrow_mut().locals.pop()?.unwrap();
            processed_variables.push((locals_len - 1, local.is_captured));

            locals_len -= 1;
            if locals_len > 0 {
                current_depth = self.inner.borrow().locals.at(locals_len - 1).depth;
            } else {
                break;
            }
        }

        processed_variables.reverse();
        Ok(processed_variables)
    }

    pub fn add_up_value(&mut self, local_addr: usize, is_local: bool) -> Result<usize> {
        self.inner.borrow_mut().up_values.add(local_addr, is_local)
    }

    // Traverse the chain of variables to find the up-value
    // either in the locals or in the already registered up-values.
    //
    // We use a subtle trick here which replicates the up-values of the parent on each
    // level in the child. This way we make sure to have access to them in arbitrarily
    // deeply nested closures.
    pub fn resolve_up_value(&mut self, name: &Identifier) -> Result<Option<usize>> {
        // If we are at the root of the variables, we reached the top-level
        // in which case the variable is global; hence not an up-value
        if self.is_root() {
            return Ok(None);
        }

        // We first check if the variable is a local in the current scope
        let addr = self.resolve_local(name);
        if let Some(local_addr) = addr {
            // It's local, so we mark it as captured. This information will be used
            // to emit the correct instruction to close it, later.
            //
            // Finally we track the up-value in the current scope (in the up-values buffer)
            self.inner
                .borrow_mut()
                .locals
                .mark_as_captured(local_addr)?;
            Ok(Some(self.add_up_value(local_addr, true)?))
        } else {
            // It's not a local in this scope, so we check if it is an up-value
            // in the parent scope.
            let mut parent = self.inner.borrow().parent.as_ref().unwrap().clone();
            let found_up_value_addr = parent.resolve_up_value(name)?;

            if let Some(upvalue_addr) = found_up_value_addr {
                // We found the up-value and replicate that state of that up-value
                // from the parent into this scope.
                self.add_up_value(upvalue_addr, false).map(Some)
            } else {
                Ok(None)
            }
        }
    }

    pub fn add_local(&mut self, name: Identifier) -> Result<usize> {
        let scope_depth = self.inner.borrow().scope_depth;
        self.inner.borrow_mut().locals.add(name, scope_depth)?;

        if let Some(addr) = self.inner.borrow().locals.last_address() {
            Ok(addr)
        } else {
            Err(Error::CompilerBug(String::from(
                "Can't find address of added local",
            )))
        }
    }

    #[inline]
    pub fn resolve_local(&self, name: &Identifier) -> Option<usize> {
        self.inner.borrow().locals.resolve(name)
    }

    #[inline]
    pub fn mark_local_as_captured(&mut self, address: usize) -> Result<()> {
        self.inner.borrow_mut().locals.mark_as_captured(address)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolve_local_finds_local_in_top_level() {
        let mut top_level = Variables::top_level();
        top_level.add_local(Identifier::synthetic("x")).unwrap();
        let local_address = top_level.resolve_local(&Identifier::synthetic("x"));

        assert_eq!(local_address, Some(0));
        assert_eq!(
            top_level.resolve_local(&Identifier::synthetic("not_there")),
            None
        );
    }

    #[test]
    fn resolve_up_value_finds_up_value_in_ancestor() {
        let top_level = Variables::top_level();
        let mut child = Variables::child(Some(top_level));
        child.add_local(Identifier::synthetic("child_x")).unwrap();
        let mut grand_child = Variables::child(Some(child));
        grand_child
            .add_local(Identifier::synthetic("grand_child_x"))
            .unwrap();
        let mut great_grand_child = Variables::child(Some(grand_child));

        // resolve the local by traversing up to the parent
        let up_value_address = great_grand_child
            .resolve_up_value(&Identifier::synthetic("child_x"))
            .unwrap();

        assert_eq!(up_value_address, Some(0));
    }

    #[test]
    fn resolve_up_value_ignores_top_level() {
        let mut top_level = Variables::top_level();
        top_level.add_local(Identifier::synthetic("outer")).unwrap();

        let mut child = Variables::child(Some(top_level));
        child.add_local(Identifier::synthetic("child_x")).unwrap();
        let mut grand_child = Variables::child(Some(child));

        assert_eq!(
            grand_child
                .resolve_up_value(&Identifier::synthetic("outer"))
                .unwrap(),
            None
        );
    }

    #[test]
    fn variabels_end_scope_identifies_captured_variables() {
        let mut vars = Variables::top_level();

        vars.begin_scope();

        vars.add_local(Identifier::synthetic("foo")).unwrap();

        let addr = vars.add_local(Identifier::synthetic("bar")).unwrap();

        vars.mark_local_as_captured(addr).unwrap();

        vars.add_local(Identifier::synthetic("baz")).unwrap();

        vars.add_local(Identifier::synthetic("baxz")).unwrap();

        let processed = vars.end_scope().unwrap();

        assert_eq!(
            processed,
            vec![(0, false), (1, true), (2, false), (3, false)]
        )
    }
}
