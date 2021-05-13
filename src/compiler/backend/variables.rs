// Variables and variables tracking during the compilation phases

pub mod locals;
pub mod up_values;

use crate::compiler::backend::code_generator::Result;
use crate::compiler::frontend::parser::expression::identifier::Identifier;
use crate::vm::byte_code::chunk::ConstAddressType;
use locals::Locals;
use std::{cell::RefCell, rc::Rc};
use up_values::UpValues;

const MAX_LOCALS: usize = 256;
const MAX_UP_VALUES: usize = MAX_LOCALS * 256;

pub type VariablesRef = Rc<RefCell<Variables>>;

pub struct Variables {
    pub parent: Option<VariablesRef>,
    pub locals: Locals,
    pub up_values: UpValues,
    pub scope_depth: usize,
}

impl Variables {
    pub fn child(parent: Option<VariablesRef>) -> VariablesRef {
        Rc::new(RefCell::new(Variables {
            scope_depth: 0,
            locals: Locals::new(MAX_LOCALS),
            up_values: UpValues::new(MAX_UP_VALUES),
            parent,
        }))
    }

    pub fn root() -> VariablesRef {
        Self::child(None)
    }

    pub fn is_root(&self) -> bool {
        self.parent.is_none()
    }

    pub fn is_top_level(&self) -> bool {
        self.scope_depth == 0
    }

    pub fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    // Close the current scope and return information about the discarded variables
    // Returns a Vec<bool> where each true value indicates that a captured variable has been popped
    // and a false value means that the variable was not captured
    pub fn end_scope(&mut self) -> Result<Vec<(ConstAddressType, bool)>> {
        self.scope_depth -= 1;

        let mut locals_len = self.locals.len();
        let mut current_depth = self.locals.at(locals_len - 1).depth;
        let mut processed_variables: Vec<(ConstAddressType, bool)> = vec![];

        //println!("Ending scope with: {:#?}", self.locals);

        while locals_len > 0 && current_depth > self.scope_depth {
            let local = self.locals.pop()?.unwrap();
            processed_variables.push(((locals_len - 1) as ConstAddressType, local.is_captured));

            locals_len -= 1;
            current_depth = self.locals.at(locals_len - 1).depth;
        }

        processed_variables.reverse();
        Ok(processed_variables)
    }

    pub fn add_up_value(
        &mut self,
        local_addr: ConstAddressType,
        is_local: bool,
    ) -> Result<ConstAddressType> {
        self.up_values.add(local_addr as usize, is_local)
    }

    pub fn resolve_up_value(&mut self, name: &Identifier) -> Result<Option<ConstAddressType>> {
        if self.is_root() {
            return Ok(None);
        }
        let addr = self.resolve_local(name);

        if let Some(local_addr) = addr {
            self.locals.mark_as_captured(local_addr);
            //println!("MARKED CAPTURED: {:?}", self.locals);
            Ok(Some(self.add_up_value(local_addr, true)?))
        } else {
            let parent = self.parent.as_ref().unwrap().clone();
            let found_up_value_addr = parent.borrow_mut().resolve_up_value(name)?;

            if let Some(upvalue_addr) = found_up_value_addr {
                self.add_up_value(upvalue_addr, false).map(Some)
            } else {
                Ok(None)
            }
        }
    }

    #[inline]
    pub fn add_local(&mut self, name: Identifier) -> Result<ConstAddressType> {
        self.locals.add(name, self.scope_depth)?;
        Ok(self.locals.last_address())
    }

    #[inline]
    pub fn resolve_local(&self, name: &Identifier) -> Option<ConstAddressType> {
        self.locals.resolve(name).map(|i| i as ConstAddressType)
    }

    #[inline]
    pub fn mark_local_as_captured(&mut self, address: ConstAddressType) -> Result<()> {
        Ok(self.locals.mark_as_captured(address))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_variabels_end_scope() {
        let vars = Variables::root();

        vars.borrow_mut().begin_scope();

        vars.borrow_mut()
            .add_local(Identifier::synthetic("foo"))
            .unwrap();

        let addr = vars
            .borrow_mut()
            .add_local(Identifier::synthetic("bar"))
            .unwrap();

        vars.borrow_mut().mark_local_as_captured(addr).unwrap();

        vars.borrow_mut()
            .add_local(Identifier::synthetic("baz"))
            .unwrap();

        vars.borrow_mut()
            .add_local(Identifier::synthetic("baxz"))
            .unwrap();

        let processed = vars.borrow_mut().end_scope().unwrap();

        assert_eq!(
            processed,
            vec![(1, false), (2, true), (3, false), (4, false)]
        )
    }
}
