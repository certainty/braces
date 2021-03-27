use crate::compiler::frontend::parser::expression;
use crate::vm::byte_code::chunk;
use crate::vm::byte_code::OpCode;
use crate::vm::value;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum GenerationError {}

type Result<T> = std::result::Result<T, GenerationError>;

pub fn generate(ast: &expression::Expression) -> Result<chunk::Chunk> {
    let mut chunk = chunk::Chunk::new();
    // line 123
    let const_addr_lhs = chunk.write_constant(value::fixnum(42));
    let const_addr_rhs = chunk.write_constant(value::fixnum(50));

    chunk.write_opcode(OpCode::Const(const_addr_lhs));
    chunk.write_opcode(OpCode::Const(const_addr_rhs));
    let end = chunk.write_opcode(OpCode::FxAdd);
    chunk.write_line(0, end, 123);

    // line 124
    let ln = chunk.write_opcode(OpCode::Halt);
    chunk.write_line(ln, ln, 124);

    Ok(chunk)
}
