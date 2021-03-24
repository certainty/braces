use braces::vm::byte_code;
use braces::vm::byte_code::chunk;
use braces::vm::byte_code::chunk::Chunk;

fn main() {
    let mut chunk = Chunk::new();
    chunk.write(byte_code::OP_RETURN);
    chunk::disassemble(&chunk, "test chunk")
}
