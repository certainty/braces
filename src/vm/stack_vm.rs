use super::byte_code::{chunk, OpCode};
use super::disassembler;
use super::error::VmError;
use super::printer;
use super::value::numeric;
use super::value::Value;

const STACK_CAPACITY: usize = 255;

pub struct StackVM<'a> {
    ip: chunk::AddressType,
    stack: Vec<Value>,
    chunk: &'a chunk::Chunk,
}

pub type Result<T> = std::result::Result<T, VmError>;

impl<'a> StackVM<'a> {
    pub fn interprete(chunk: &'a chunk::Chunk) -> Result<Option<Value>> {
        StackVM {
            chunk: &chunk,
            ip: 0,
            stack: Vec::with_capacity(STACK_CAPACITY),
        }
        .run()
    }

    fn run(&mut self) -> Result<Option<Value>> {
        loop {
            #[cfg(feature = "debug_vm")]
            self.debug_cycle();

            match self.read_op_code() {
                &OpCode::Halt => {
                    return Ok(Some(self.pop()));
                }
                &OpCode::Const(addr) => {
                    let val = self.chunk.read_constant(addr);
                    self.stack.push(val.clone());
                }
                &OpCode::Apply => todo!(),
                &OpCode::Nop => {
                    //this should never happen
                    panic!("BUG in the compiler detected");
                }
            }
        }
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn pop_n(&mut self, size: usize) -> Vec<Value> {
        let mut result: Vec<Value> = Vec::with_capacity(size);

        for _ in 1..size {
            result.push(self.pop());
        }

        result
    }

    fn read_op_code(&mut self) -> &OpCode {
        let code = self.chunk.read_opcode(self.ip);
        self.ip = self.ip + 1;
        code
    }

    fn debug_cycle(&self) {
        self.print_stack_trace();
        disassembler::disassemble_instruction(&mut std::io::stdout(), self.chunk, self.ip);
    }

    fn print_stack_trace(&self) {
        print!("     ");
        for value in self.stack.iter() {
            print!("[{}]", printer::print(value));
        }
        println!("")
    }
}
