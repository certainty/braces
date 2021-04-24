use crate::vm::byte_code::chunk::Chunk;

#[derive(Debug, Clone)]
pub enum Arity {
    Fixed(usize),
    FixedWithRest(usize),
    Variadic,
}

#[derive(Debug, Clone)]
pub struct NamedLambda {
    pub lambda: Lambda,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub arity: Arity,
    pub chunk: Chunk,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Procedure {
    Named(NamedLambda),
    Lambda(Lambda),
}

// for now no procedure is equal unless it is itself
impl PartialEq for Lambda {
    fn eq(&self, other: &Lambda) -> bool {
        (self as *const _) == (other as *const _)
    }
}

impl PartialEq for NamedLambda {
    fn eq(&self, other: &NamedLambda) -> bool {
        (self as *const _) == (other as *const _)
    }
}
