use super::byte_code::chunk::{AddressType, Chunk};
use super::byte_code::Instruction;
#[cfg(feature = "debug_vm")]
use super::disassembler::Disassembler;
use super::scheme::value::Value;
use super::Error;

type Result<T> = std::result::Result<T, Error>;

pub struct Instance<'a> {
    ip: AddressType,
    current_chunk: &'a Chunk,
    stack: Vec<Value>,
    #[cfg(feature = "debug_vm")]
    disassembler: Disassembler<std::io::Stdout>,
}

impl<'a> Instance<'a> {
    pub fn interprete(chunk: &Chunk, stack_size: usize) -> Result<Value> {
        Instance {
            stack: Vec::with_capacity(stack_size),
            current_chunk: &chunk,
            ip: 0,
            #[cfg(feature = "debug_vm")]
            disassembler: Disassembler::new(std::io::stdout()),
        }
        .run()
    }

    fn run(&mut self) -> Result<Value> {
        #[cfg(feature = "debug_vm")]
        self.disassembler
            .disassemble(self.current_chunk, "DEBUG DISASS");

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

    #[cfg(feature = "debug_vm")]
    fn debug_cycle(&mut self) {
        self.print_stack();
        self.disassembler
            .disassemble_instruction(&self.current_chunk, self.ip);
    }

    #[cfg(feature = "debug_vm")]
    fn print_stack(&self) {
        print!("     ");
        for value in self.stack.iter() {
            print!("[{:?}]", value);
        }
        println!("")
    }
}
