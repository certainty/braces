use crate::compiler::frontend::parser::expression;
use crate::compiler::source::SourceInformation;
use crate::vm::byte_code::chunk;
use crate::vm::byte_code::OpCode;
use crate::vm::value;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum GenerationError {
    #[error("Unknown expression encountered")]
    UnknownExpression(expression::Expression),
}

type Result<T> = std::result::Result<T, GenerationError>;

pub fn generate(ast: &expression::Expression) -> Result<chunk::Chunk> {
    let mut chunk = chunk::Chunk::new();
    emit_op_codes(&mut chunk, ast)?;
    Ok(chunk)

    /*
    // line 123
    let const_addr_lhs = chunk.write_constant(value::fixnum(42));
    let const_addr_rhs = chunk.write_constant(value::fixnum(50));

    chunk.write_opcode(OpCode::Const(const_addr_lhs));
    chunk.write_opcode(OpCode::Const(const_addr_rhs));
    let end = chunk.write_opcode(OpCode::FxAdd);
    chunk.write_line(0, end, 123);

    // line 124
    let ln = chunk.write_opcode(OpCode::Halt);
    chunk.write_line(ln, ln, 124); */
}

pub fn finalise(chunk: &mut chunk::Chunk) {
    chunk.write_opcode(OpCode::Halt);
    chunk.write_opcode(OpCode::Nop);
}

fn emit_op_codes(chunk: &mut chunk::Chunk, ast: &expression::Expression) -> Result<()> {
    match ast {
        expression::Expression::Literal(value, source) => emit_literal(chunk, *value, &source),
        _ => Err(GenerationError::UnknownExpression((*ast).clone())),
    }
}

fn emit_literal(
    chunk: &mut chunk::Chunk,
    value: value::Value,
    source: &SourceInformation,
) -> Result<()> {
    let addr = chunk.write_constant(value);
    let caddr = chunk.write_opcode(OpCode::Const(addr));
    chunk.write_line(caddr.into(), caddr.into(), source.location.line);
    Ok(())
}
