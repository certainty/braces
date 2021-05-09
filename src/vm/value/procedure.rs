use super::equality::SchemeEqual;
use super::lambda;
use crate::vm::byte_code::chunk::Chunk;

#[derive(Debug, Clone)]
pub enum Arity {
    Exactly(usize),
    AtLeast(usize),
    Many,
}

pub trait HasArity {
    fn arity<'a>(&'a self) -> &'a Arity;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Procedure {
    Named(lambda::NamedLambda),
    Lambda(lambda::Lambda),
}

impl Procedure {
    pub fn code<'a>(&'a self) -> &'a Chunk {
        match self {
            Procedure::Named(proc) => &proc.lambda.chunk,
            Procedure::Lambda(proc) => &proc.chunk,
        }
    }

    pub fn name(&self) -> String {
        if let Self::Named(named) = self {
            named.name.clone()
        } else {
            String::from("")
        }
    }

    pub fn up_value_count(&self) -> usize {
        match self {
            Procedure::Named(proc) => proc.lambda.up_value_count,
            Procedure::Lambda(proc) => proc.up_value_count,
        }
    }
}

impl SchemeEqual<Procedure> for Procedure {
    fn is_eq(&self, other: &Procedure) -> bool {
        match (self, other) {
            (Self::Named(lhs), Self::Named(rhs)) => lhs.is_eq(rhs),
            (Self::Lambda(lhs), Self::Lambda(rhs)) => lhs.is_eq(rhs),
            _ => false,
        }
    }

    fn is_eqv(&self, other: &Procedure) -> bool {
        match (self, other) {
            (Self::Named(lhs), Self::Named(rhs)) => lhs.is_eqv(rhs),
            (Self::Lambda(lhs), Self::Lambda(rhs)) => lhs.is_eqv(rhs),
            _ => false,
        }
    }

    fn is_equal(&self, other: &Procedure) -> bool {
        match (self, other) {
            (Self::Named(lhs), Self::Named(rhs)) => lhs.is_equal(rhs),
            (Self::Lambda(lhs), Self::Lambda(rhs)) => lhs.is_equal(rhs),
            _ => false,
        }
    }
}

impl HasArity for Procedure {
    fn arity<'a>(&'a self) -> &'a Arity {
        match self {
            Procedure::Named(proc) => &proc.lambda.arity,
            Procedure::Lambda(proc) => &proc.arity,
        }
    }
}

impl HasArity for std::rc::Rc<Procedure> {
    fn arity<'a>(&'a self) -> &'a Arity {
        match &**self {
            Procedure::Named(proc) => &proc.lambda.arity,
            Procedure::Lambda(proc) => &proc.arity,
        }
    }
}

pub fn thunk(chunk: Chunk, up_value_count: usize) -> Procedure {
    lambda(Arity::Exactly(0), chunk, up_value_count)
}

pub fn lambda(arity: Arity, chunk: Chunk, up_value_count: usize) -> Procedure {
    Procedure::Lambda(lambda::Lambda {
        arity,
        chunk,
        up_value_count,
    })
}

pub fn named(name: String, arity: Arity, chunk: Chunk, up_value_count: usize) -> Procedure {
    let lambda = lambda::Lambda {
        arity,
        chunk,
        up_value_count,
    };
    Procedure::Named(lambda::NamedLambda { name, lambda })
}
