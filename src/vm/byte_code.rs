pub mod chunk;
use chunk::{AddressType, ConstAddressType};

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum Instruction {
    Define(ConstAddressType),
    SetGlobal(ConstAddressType),
    SetUpValue(ConstAddressType),
    SetLocal(ConstAddressType),
    GetGlobal(ConstAddressType),
    GetUpValue(ConstAddressType),
    GetLocal(ConstAddressType),
    Const(ConstAddressType),
    Closure(ConstAddressType),
    UpValue(ConstAddressType, bool),
    JumpIfFalse(AddressType),
    Jump(AddressType),
    Call(usize), // number of arguments
    Break,       // Reserved for future use
    Return,
    Nop, // do nothing
    Pop,
    Nil,
    True,
    False,
}
