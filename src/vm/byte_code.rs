pub mod chunk;

pub type ConstAddressType = u16;

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum OpCode {
    Halt,
    Nop,
    Apply,
    Get,
    Const(ConstAddressType),
}
