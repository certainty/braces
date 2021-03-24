use braces::vm::byte_code::chunk;
use braces::vm::byte_code::chunk::Chunk;
use braces::vm::byte_code::OpCode;
use braces::vm::disassembler::disassemble;

fn main() {
    let mut chunk = Chunk::new();
    chunk.write_opcode(OpCode::Return);
    disassemble(&chunk, "test chunk")
}
