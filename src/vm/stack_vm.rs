use super::byte_code::{chunk, OpCode};
use super::disassembler;
use super::environment;
use super::error::VmError;
use super::printer;
use super::value;
use super::value::Value;

const FRAMES_MAX: usize = 64;
const STACK_CAPACITY: usize = 255;
const STACK_MAX: usize = FRAMES_MAX + STACK_CAPACITY;

pub struct CallFrame<'a> {
    callable: &'a value::Procedure,
    //    env: &'a environment::Environment,
    ip: chunk::AddressType,
}

pub struct StackVM<'a> {
    frames: Vec<CallFrame<'a>>,
    frame: CallFrame<'a>,
    stack: Vec<Value>,
}

pub type Result<T> = std::result::Result<T, VmError>;

impl<'a> StackVM<'a> {
    pub fn interprete(chunk: chunk::Chunk) -> Result<Option<Value>> {
        let frame = CallFrame {
            callable: &value::Procedure {
                arity: 0,
                chunk: chunk,
            },
            ip: 0,
        };

        StackVM {
            stack: Vec::with_capacity(STACK_CAPACITY),
            frames: vec![],
            frame: frame,
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
                    let val = self.frame.callable.chunk.read_constant(addr);
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
        let code = self.frame.callable.chunk.read_opcode(self.frame.ip);
        self.frame.ip = self.frame.ip + 1;
        code
    }

    fn debug_cycle(&self) {
        self.print_stack_trace();
        disassembler::disassemble_instruction(
            &mut std::io::stdout(),
            &self.frame.callable.chunk,
            self.frame.ip,
        );
    }

    fn print_stack_trace(&self) {
        print!("     ");
        for value in self.stack.iter() {
            print!("[{}]", printer::print(value));
        }
        println!("")
    }
}
