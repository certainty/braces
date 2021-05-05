use crate::vm::byte_code::chunk::Chunk;

#[derive(Debug, Clone)]
pub enum Arity {
    Exactly(usize),
    AtLeast(usize),
    Many,
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

impl Procedure {
    pub fn code<'a>(&'a self) -> &'a Chunk {
        match self {
            Procedure::Named(proc) => &proc.lambda.chunk,
            Procedure::Lambda(proc) => &proc.chunk,
        }
    }

    pub fn arity<'a>(&'a self) -> &'a Arity {
        match self {
            Procedure::Named(proc) => &proc.lambda.arity,
            Procedure::Lambda(proc) => &proc.arity,
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

impl Procedure {
    pub fn thunk(chunk: Chunk) -> Procedure {
        Self::lambda(Arity::Exactly(0), chunk)
    }

    pub fn lambda(arity: Arity, chunk: Chunk) -> Procedure {
        Procedure::Lambda(Lambda { arity, chunk })
    }

    pub fn named(name: String, arity: Arity, chunk: Chunk) -> Procedure {
        let lambda = Lambda { arity, chunk };
        Procedure::Named(NamedLambda { name, lambda })
    }
}
