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
            String::from("lambda")
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

pub fn thunk(chunk: Chunk) -> Procedure {
    lambda(Arity::Exactly(0), chunk)
}

pub fn lambda(arity: Arity, chunk: Chunk) -> Procedure {
    Procedure::Lambda(lambda::Lambda { arity, chunk })
}

pub fn named(name: String, arity: Arity, chunk: Chunk) -> Procedure {
    let lambda = lambda::Lambda { arity, chunk };
    Procedure::Named(lambda::NamedLambda { name, lambda })
}
