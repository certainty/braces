pub mod chunk;
use super::value::symbol::InternedSymbol;
pub type ConstAddressType = u16;

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum OpCode {
    Halt,
    Nop,
    Apply,
    Get,
    Sym(InternedSymbol),
    Const(ConstAddressType),
}
