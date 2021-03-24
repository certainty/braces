pub mod chunk;

type OpCode = u8;

pub const OP_RETURN: OpCode = 0;
pub const OP_CONSTANT: OpCode = 1;
