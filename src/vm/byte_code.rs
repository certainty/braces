pub mod chunk;
use super::scheme::value::Value;

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum Instruction {
    Const(Value),
}
