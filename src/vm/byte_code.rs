pub mod chunk;

pub type ConstAddressType = u16;

#[repr(u8)]
#[derive(Debug)]
pub enum OpCode {
    Halt,
    Nop,
    Const(ConstAddressType),
    FxAdd,
}
