use braces::vm::byte_code::chunk::Chunk;
use braces::vm::byte_code::OpCode;
use braces::vm::disassembler::disassemble;
use braces::vm::stack_vm::StackVM;

fn main() {
    pretty_env_logger::init();
    let mut chunk = Chunk::new();

    // line 123
    let const_addr_lhs = chunk.write_constant(42);
    let const_addr_rhs = chunk.write_constant(50);

    chunk.write_opcode(OpCode::Const(const_addr_lhs));
    chunk.write_opcode(OpCode::Const(const_addr_rhs));
    let end = chunk.write_opcode(OpCode::FxAdd);
    chunk.write_line(0, end, 123);

    // line 124
    let ln = chunk.write_opcode(OpCode::Exit);
    chunk.write_line(ln, ln, 124);

    disassemble(&mut std::io::stdout(), &chunk, "test chunk");

    println!("\nrunning VM with the code above");
    StackVM::interprete(&chunk).unwrap();
}
