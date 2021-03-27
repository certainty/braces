use super::byte_code::{chunk, OpCode};
use super::error::VmError;
use super::printer;
use super::value::numeric;
use super::value::Value;

#[cfg(feature = "debug_vm")]
use super::disassembler;

const STACK_CAPACITY: usize = 255;

pub struct StackVM<'a> {
    ip: chunk::AddressType,
    stack: Vec<Value>,
    chunk: &'a chunk::Chunk,
}

impl<'a> StackVM<'a> {
    pub fn interprete(chunk: &'a chunk::Chunk) -> Result<Option<Value>, VmError> {
        StackVM {
            chunk: &chunk,
            ip: 0,
            stack: Vec::with_capacity(STACK_CAPACITY),
        }
        .run()
    }

    fn run(&mut self) -> Result<Option<Value>, VmError> {
        loop {
            #[cfg(feature = "debug_vm")]
            self.debug_cycle();

            match self.read_op_code() {
                &OpCode::Halt => {
                    let v = self.stack.pop();
                    return Ok(v);
                }
                &OpCode::Const(addr) => {
                    let val = self.chunk.read_constant(addr);
                    self.stack.push(val);
                }
                &OpCode::FxAdd => {
                    let lhs = self.stack.pop().unwrap();
                    let rhs = self.stack.pop().unwrap();
                    let result = numeric::add(lhs, rhs)?;
                    self.stack.push(Value::Number(result))
                }
            }
        }
    }

    fn read_op_code(&mut self) -> &OpCode {
        let code = self.chunk.read_opcode(self.ip);
        self.ip = self.ip + 1;
        code
    }

    #[cfg(feature = "debug_vm")]
    fn debug_cycle(&self) {
        self.print_stack_trace();
        disassembler::disassemble_instruction(&mut std::io::stdout(), self.chunk, self.ip);
    }

    #[cfg(feature = "debug_vm")]
    fn print_stack_trace(&self) {
        print!("     ");
        for value in self.stack.iter() {
            print!("[{}]", printer::print(value));
        }
        println!("")
    }
}
