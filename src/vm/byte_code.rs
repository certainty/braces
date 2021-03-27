pub mod chunk;

pub type ConstAddressType = u16;

#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    Halt,
    Nop,
    Apply,
    Const(ConstAddressType),
}
