pub mod chunk;

pub type ConstAddressType = u16;

#[repr(u8)]
pub enum OpCode {
    Exit,
    Const(ConstAddressType),
    FxAdd,
}
