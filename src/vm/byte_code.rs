pub mod chunk;
use chunk::ConstAddressType;

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum Instruction {
    Const(ConstAddressType),
    Halt,
}
