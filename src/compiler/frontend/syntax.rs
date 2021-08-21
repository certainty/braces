use crate::vm::value::procedure;

pub mod environment;

pub mod symbol;

#[derive(Debug, Clone)]
pub enum Transformer {
    ExplicitRenaming(procedure::Procedure),
}
