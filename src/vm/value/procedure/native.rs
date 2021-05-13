use super::Arity;
use crate::vm::byte_code::chunk::Chunk;
use crate::vm::value::equality::SchemeEqual;

#[derive(Debug, Clone)]
pub struct Procedure {
    pub name: Option<String>,
    pub up_value_count: usize,
    pub arity: Arity,
    pub chunk: Chunk,
}

impl Procedure {
    pub fn thunk(chunk: Chunk, up_value_count: usize) -> Self {
        Self::lambda(Arity::Exactly(0), chunk, up_value_count)
    }

    pub fn named(name: String, arity: Arity, chunk: Chunk, up_value_count: usize) -> Self {
        Self {
            name: Some(name),
            arity,
            chunk,
            up_value_count,
        }
    }

    pub fn lambda(arity: Arity, chunk: Chunk, up_value_count: usize) -> Self {
        Self {
            name: None,
            arity,
            chunk,
            up_value_count,
        }
    }

    pub fn code<'a>(&'a self) -> &'a Chunk {
        &self.chunk
    }

    pub fn name<'a>(&'a self) -> &'a Option<String> {
        &self.name
    }
}

// for now no procedure is equal unless it is itself
impl PartialEq for Procedure {
    fn eq(&self, other: &Procedure) -> bool {
        (self as *const _) == (other as *const _)
    }
}

impl SchemeEqual<Procedure> for Procedure {
    fn is_eq(&self, other: &Procedure) -> bool {
        self == other
    }

    fn is_eqv(&self, other: &Procedure) -> bool {
        self == other
    }

    fn is_equal(&self, other: &Procedure) -> bool {
        self == other
    }
}
