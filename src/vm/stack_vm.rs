use super::byte_code::{chunk, OpCode};
use super::disassembler;
use super::environment;
use super::error::VmError;
use super::printer;
use super::value::procedure;
use super::value::Value;

const FRAMES_MAX: usize = 64;
const STACK_CAPACITY: usize = 255;
const STACK_MAX: usize = FRAMES_MAX + STACK_CAPACITY;

pub type Result<T> = std::result::Result<T, VmError>;

pub struct CallFrame {
    procedure: procedure::Lambda,
    env: environment::Environment,
}

impl CallFrame {
    pub fn root(chunk: chunk::Chunk, root_env: environment::Environment) -> Self {
        Self {
            procedure: procedure::Lambda::thunk(chunk),
            env: root_env,
        }
    }
}

pub struct StackVM<'a> {
    ip: chunk::AddressType,
    current_chunk: &'a chunk::Chunk,
    top_frame: &'a CallFrame,
    frames: Vec<CallFrame>,
    stack: Vec<Value>,
}

impl<'a> StackVM<'a> {
    pub fn interprete(chunk: chunk::Chunk) -> Result<Option<Value>> {
        let root_env = environment::Environment::empty();
        let frame = CallFrame::root(chunk, root_env);

        StackVM {
            stack: Vec::with_capacity(STACK_CAPACITY),
            frames: vec![],
            current_chunk: &frame.procedure.chunk,
            top_frame: &frame,
            ip: 0,
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
                    let val = self.current_chunk.read_constant(addr);
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
        let code = self.current_chunk.read_opcode(self.ip);
        self.ip = self.ip + 1;
        code
    }

    fn debug_cycle(&self) {
        self.print_stack_trace();
        disassembler::disassemble_instruction(&mut std::io::stdout(), &self.current_chunk, self.ip);
    }

    fn print_stack_trace(&self) {
        print!("     ");
        for value in self.stack.iter() {
            print!("[{}]", printer::print(value));
        }
        println!("")
    }
}
