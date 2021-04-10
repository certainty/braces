pub mod chunk;
use chunk::ConstAddressType;

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum Instruction {
    Set(ConstAddressType),
    Get(ConstAddressType),
    Const(ConstAddressType),
    Halt,
    Nil,
    True,
    False,
}
