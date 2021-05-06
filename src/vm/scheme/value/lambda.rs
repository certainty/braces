use super::procedure::Arity;
use crate::vm::byte_code::chunk::Chunk;
use crate::vm::scheme::equality::SchemeEqual;

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

impl SchemeEqual<NamedLambda> for NamedLambda {
    fn is_eq(&self, other: &NamedLambda) -> bool {
        self == other
    }

    fn is_eqv(&self, other: &NamedLambda) -> bool {
        self == other
    }

    fn is_equal(&self, other: &NamedLambda) -> bool {
        self == other
    }
}

impl SchemeEqual<Lambda> for Lambda {
    fn is_eq(&self, other: &Lambda) -> bool {
        self == other
    }

    fn is_eqv(&self, other: &Lambda) -> bool {
        self == other
    }

    fn is_equal(&self, other: &Lambda) -> bool {
        self == other
    }
}
