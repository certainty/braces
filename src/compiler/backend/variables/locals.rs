use crate::compiler::backend::code_generator::{Error, Result};
use crate::compiler::frontend::expression::identifier::Identifier;

#[derive(Debug, Clone, PartialEq)]
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct Locals {
    max: usize,
    locals: Vec<Local>,
}

impl Locals {
    pub fn new(limit: usize) -> Self {
        Self {
            max: limit,
            locals: Vec::with_capacity(limit),
        }
    }

    pub fn at<'a>(&'a self, idx: usize) -> &'a Local {
        &self.locals[idx]
    }

    pub fn mark_as_captured(&mut self, idx: usize) -> Result<()> {
        if idx >= self.len() {
            return Err(Error::CompilerBug(String::from(
                "Local index out of ranged",
            )));
        }

        let existing = self.locals[idx].clone();
        self.locals[idx] = Local::capture(existing);

        Ok(())
    }

    // Add a local to the specified scope_depth
    //
    // If more locals are tracked than the limit allows it returns `Error::TooManyLocals`
    // otherwise it adds the local as non-captured.
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

    pub fn last_address(&self) -> Option<usize> {
        if self.locals.len() > 0 {
            Some(self.locals.len() - 1)
        } else {
            None
        }
    }

    pub fn len(&self) -> usize {
        self.locals.len()
    }

    // Find the index of last local that has been defined with this name.
    pub fn resolve(&self, id: &Identifier) -> Option<usize> {
        self.locals.iter().rposition(|l| l.name == *id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mark_as_captured_marks_as_captured() {
        let mut locals = Locals::new(10);
        locals.add(Identifier::synthetic("foo"), 1).unwrap();

        assert!(
            !locals.at(0).is_captured,
            "Expected local not to be captured"
        );

        locals.mark_as_captured(0).unwrap();

        assert!(locals.at(0).is_captured, "Expected local to be captured");
    }

    #[test]
    fn mark_as_captured_detects_compiler_bug() {
        let mut locals = Locals::new(10);
        assert!(locals.mark_as_captured(1).is_err(), "Expected error")
    }

    #[test]
    fn last_address() {
        let mut locals = Locals::new(10);

        assert_eq!(locals.last_address(), None);

        locals.add(Identifier::synthetic("foo"), 1).unwrap();

        assert_eq!(locals.last_address(), Some(0));
    }

    #[test]
    fn add_locals_adds_upcaptured() {
        let mut locals = Locals::new(10);

        locals.add(Identifier::synthetic("foo"), 1).unwrap();
        locals.add(Identifier::synthetic("bar"), 1).unwrap();

        assert!(
            !locals.at(0).is_captured,
            "Expected local not to be captured"
        );
    }

    #[test]
    fn resolve_local_finds_first_matching_locals() {
        let mut locals = Locals::new(10);

        locals.add(Identifier::synthetic("foo"), 1).unwrap();
        locals.add(Identifier::synthetic("bar"), 1).unwrap();
        locals.add(Identifier::synthetic("barz"), 1).unwrap();
        locals.add(Identifier::synthetic("bar"), 1).unwrap();

        assert_eq!(locals.resolve(&Identifier::synthetic("foo")), Some(0));
        assert_eq!(locals.resolve(&Identifier::synthetic("bar")), Some(3));
        assert_eq!(locals.resolve(&Identifier::synthetic("nope")), None);
    }
}
