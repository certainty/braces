use braces::vm::byte_code::chunk::Chunk;
use braces::vm::byte_code::OpCode;
use braces::vm::disassembler::disassemble;

fn main() {
    let mut chunk = Chunk::new();
    chunk.write_opcode(OpCode::Return);
    let const_addr = chunk.write_constant(42);
    chunk.write_opcode(OpCode::Const(const_addr));

    disassemble(&chunk, "test chunk")
}
