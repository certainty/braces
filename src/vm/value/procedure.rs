pub mod foreign;
pub mod native;
use super::equality::SchemeEqual;
use crate::vm::byte_code::chunk::Chunk;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Arity {
    Exactly(usize),
    AtLeast(usize),
    Many,
}

pub trait HasArity {
    fn arity(&self) -> &Arity;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Procedure {
    Native(Rc<native::Procedure>),
    Foreign(Rc<foreign::Procedure>),
}

impl Procedure {
    pub fn foreign(proc: foreign::Procedure) -> Self {
        Self::Foreign(Rc::new(proc))
    }

    pub fn native(proc: native::Procedure) -> Self {
        Self::Native(Rc::new(proc))
    }

    pub fn name(&self) -> Option<String> {
        match self {
            Self::Native(proc) => proc.name().clone(),
            Self::Foreign(proc) => Some(proc.name.clone()),
        }
    }

    pub fn is_native(&self) -> bool {
        match self {
            Self::Native(_) => true,
            Self::Foreign(_) => false,
        }
    }

    pub fn as_native(&self) -> Rc<native::Procedure> {
        match self {
            Self::Native(proc) => proc.clone(),
            _ => panic!("Can't extract foreign procedure from closure"),
        }
    }

    pub fn up_value_count(&self) -> usize {
        match self {
            Self::Native(proc) => proc.up_value_count,
            _ => 0,
        }
    }
}

impl SchemeEqual<Procedure> for Procedure {
    fn is_eq(&self, other: &Procedure) -> bool {
        match (self, other) {
            (Self::Native(lhs), Self::Native(rhs)) => Rc::ptr_eq(lhs, rhs),
            (Self::Foreign(lhs), Self::Foreign(rhs)) => Rc::ptr_eq(lhs, rhs),
            _ => false,
        }
    }

    fn is_eqv(&self, other: &Procedure) -> bool {
        match (self, other) {
            (Self::Native(lhs), Self::Native(rhs)) => lhs.is_eq(rhs),
            (Self::Foreign(lhs), Self::Foreign(rhs)) => lhs.is_eq(rhs),
            _ => false,
        }
    }

    fn is_equal(&self, other: &Procedure) -> bool {
        match (self, other) {
            (Self::Native(lhs), Self::Native(rhs)) => lhs.is_eq(rhs),
            (Self::Foreign(lhs), Self::Foreign(rhs)) => lhs.is_eq(rhs),
            _ => false,
        }
    }
}

impl HasArity for Procedure {
    fn arity(&self) -> &Arity {
        match self {
            Procedure::Native(proc) => &proc.arity,
            Procedure::Foreign(proc) => &proc.arity,
        }
    }
}

impl HasArity for std::rc::Rc<Procedure> {
    fn arity(&self) -> &Arity {
        match &**self {
            Procedure::Native(proc) => &proc.arity,
            Procedure::Foreign(proc) => &proc.arity,
        }
    }
}

pub fn thunk(chunk: Chunk, up_value_count: usize) -> Procedure {
    Procedure::Native(Rc::new(native::Procedure::thunk(chunk, up_value_count)))
}

pub fn lambda(arity: Arity, chunk: Chunk, up_value_count: usize) -> Procedure {
    Procedure::Native(Rc::new(native::Procedure::lambda(
        arity,
        chunk,
        up_value_count,
    )))
}

pub fn named(name: String, arity: Arity, chunk: Chunk, up_value_count: usize) -> Procedure {
    Procedure::Native(Rc::new(native::Procedure::named(
        name,
        arity,
        chunk,
        up_value_count,
    )))
}
