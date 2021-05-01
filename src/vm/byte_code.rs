pub mod chunk;
use chunk::ConstAddressType;

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum Instruction {
    Define(ConstAddressType),
    Set(ConstAddressType),
    SetLocal(ConstAddressType),
    Get(ConstAddressType),
    GetLocal(ConstAddressType),
    Const(ConstAddressType),
    Call(usize), // number of arguments
    Break,       // Reserved for future use
    Return,
    Nop, // do nothing
    Pop,
    Nil,
    True,
    False,
}
