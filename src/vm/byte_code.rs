pub mod chunk;
use chunk::{AddressType, ConstAddressType};

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum Instruction {
    Const(ConstAddressType),
    Define(ConstAddressType),
    Closure(ConstAddressType),
    GetGlobal(ConstAddressType),
    GetUpValue(AddressType),
    GetLocal(AddressType),
    UpValue(AddressType, bool),
    CloseUpValue(AddressType),
    Set,
    JumpIfFalse(AddressType),
    Jump(AddressType),
    Call(usize),     // number of arguments
    TailCall(usize), // number of arguments
    Break,           // Reserved for future use
    Return,
    Nop, // do nothing
    Pop,
    Nil,
    True,
    False,
}
