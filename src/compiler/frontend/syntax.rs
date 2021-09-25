use crate::vm::value::procedure;

pub mod environment;

pub mod symbol;

#[derive(Debug, Clone)]
pub enum Transformer {
    LowLevel(procedure::Procedure),
    ExplicitRenaming(procedure::Procedure),
}
