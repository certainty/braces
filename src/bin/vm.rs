use braces::vm::byte_code::chunk::Chunk;
use braces::vm::byte_code::OpCode;
use braces::vm::disassembler::disassemble;

fn main() {
    let mut chunk = Chunk::new();

    let const_addr = chunk.write_constant(42);
    let start = chunk.write_opcode(OpCode::Const(const_addr));
    let end = chunk.write_opcode(OpCode::Return);

    chunk.write_line(start, end, 123);
    disassemble(&chunk, "test chunk");
}
