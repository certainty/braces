pub mod chunk;
use chunk::{AddressType, ConstAddressType};

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum Instruction {
    Const(ConstAddressType),
    Define(ConstAddressType),
    Closure(ConstAddressType),
    SetGlobal(ConstAddressType),
    GetGlobal(ConstAddressType),
    SetUpValue(AddressType),
    GetUpValue(AddressType),
    SetLocal(AddressType),
    GetLocal(AddressType),
    UpValue(AddressType, bool),
    CloseUpValue(AddressType),
    JumpIfFalse(AddressType),
    Jump(AddressType),
    Call(usize),     // number of arguments
    TailCall(usize), // number of arguments
    Reverse,         // reverse a list
    Cons,            // cons one element to value
    Append,
    VecPush, // push an element to a vector
    VecAppend,
    Break, // Reserved for future use
    Return,
    Nop, // do nothing
    Pop,
    Nil,
    NilVec,
    True,
    False,
}
