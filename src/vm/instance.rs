use super::byte_code::chunk::{AddressType, Chunk};
use super::byte_code::Instruction;
use super::scheme::value::Value;
use super::Error;

type Result<T> = std::result::Result<T, Error>;

pub struct Instance<'a> {
    ip: AddressType,
    current_chunk: &'a Chunk,
    stack: Vec<Value>,
}

impl<'a> Instance<'a> {
    pub fn interprete(chunk: &Chunk, stack_size: usize) -> Result<Value> {
        Instance {
            stack: Vec::with_capacity(stack_size),
            current_chunk: &chunk,
            ip: 0,
        }
        .run()
    }

    fn run(&mut self) -> Result<Value> {
        loop {
            #[cfg(feature = "debug_vm")]
            self.debug_cycle();

            match self.next_instruction() {
                &Instruction::Halt => return Ok(self.stack.pop().unwrap_or(Value::Unspecified)),
                &Instruction::Const(addr) => {
                    let value = self.current_chunk.read_constant(addr);
                    self.stack.push(value.clone());
                }
            }
        }
    }

    fn unsafe_pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn next_instruction(&mut self) -> &Instruction {
        let instruction = self.current_chunk.read_instruction(self.ip);
        self.ip = self.ip + 1;
        instruction
    }

    fn debug_cycle(&self) {
        self.print_stack();
        //disassembler::disassemble_instruction(&mut std::io::stdout(), &self.current_chunk, self.ip);
    }

    fn print_stack(&self) {
        print!("     ");
        for value in self.stack.iter() {
            print!("[{:?}]", value);
        }
        println!("")
    }
}
