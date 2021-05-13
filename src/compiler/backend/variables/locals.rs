use crate::compiler::backend::code_generator::{Error, Result};
use crate::compiler::frontend::parser::expression::identifier::Identifier;
use crate::vm::byte_code::chunk::ConstAddressType;

#[derive(Debug, Clone)]
pub struct Local {
    pub name: Identifier,
    pub depth: usize,
    pub is_captured: bool,
}

impl Local {
    pub fn new(name: Identifier, scope: usize, is_captured: bool) -> Self {
        Local {
            name,
            depth: scope,
            is_captured,
        }
    }

    pub fn capture(other: Local) -> Self {
        Local {
            name: other.name,
            depth: other.depth,
            is_captured: true,
        }
    }

    pub fn for_vm() -> Self {
        Local {
            name: Identifier::synthetic(""),
            depth: 0,
            is_captured: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Locals {
    max: usize,
    locals: Vec<Local>,
}

impl Locals {
    pub fn new(limit: usize) -> Self {
        let mut i = Self {
            max: limit,
            locals: Vec::with_capacity(limit),
        };

        // first slot is reserved for the vm
        i.locals.push(Local::for_vm());
        i
    }

    pub fn at<'a>(&'a self, idx: usize) -> &'a Local {
        &self.locals[idx]
    }

    pub fn mark_as_captured(&mut self, addr: ConstAddressType) {
        let idx = addr as usize;
        let existing = self.locals[idx].clone();
        self.locals[idx] = Local::capture(existing);
    }

    pub fn add(&mut self, name: Identifier, scope_depth: usize) -> Result<()> {
        if self.locals.len() >= self.max {
            Err(Error::TooManyLocals)
        } else {
            self.locals.push(Local::new(name, scope_depth, false));
            Ok(())
        }
    }

    pub fn pop(&mut self) -> Result<Option<Local>> {
        Ok(self.locals.pop())
    }

    pub fn last_address(&self) -> ConstAddressType {
        (self.locals.len() - 1) as ConstAddressType
    }

    pub fn len(&self) -> usize {
        self.locals.len()
    }

    pub fn resolve(&self, id: &Identifier) -> Option<usize> {
        self.locals.iter().rposition(|l| l.name == *id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolve_local() {
        let mut locals = Locals::new(10);

        locals.add(Identifier::synthetic("foo"), 1).unwrap();
        locals.add(Identifier::synthetic("bar"), 1).unwrap();
        locals.add(Identifier::synthetic("barz"), 1).unwrap();

        assert_eq!(locals.resolve(&Identifier::synthetic("foo")), Some(1))
    }
}
