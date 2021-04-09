use super::byte_code::chunk::{AddressType, Chunk};
use super::byte_code::Instruction;
#[cfg(feature = "debug_vm")]
use super::disassembler::Disassembler;
use super::scheme::value::Value;
use super::Error;
use rustc_hash::FxHashMap;

type Result<T> = std::result::Result<T, Error>;

struct StringTable {
    strings: FxHashMap<String, Value>,
}

impl StringTable {
    pub fn new() -> Self {
        Self {
            strings: FxHashMap::default(),
        }
    }

    pub fn intern(&mut self, s: impl Into<String>) -> &Value {
        let k = s.into();
        let v = k.clone();

        self.strings.entry(k).or_insert(Value::string(v))
    }
}

pub struct Instance<'a> {
    ip: AddressType,
    current_chunk: &'a Chunk,
    strings: StringTable,
    stack: Vec<Value>,
    #[cfg(feature = "debug_vm")]
    disassembler: Disassembler<std::io::Stdout>,
}

impl<'a> Instance<'a> {
    pub fn interprete(chunk: &Chunk, stack_size: usize) -> Result<Value> {
        Instance {
            stack: Vec::with_capacity(stack_size),
            strings: StringTable::new(),
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
                &Instruction::True => self.stack.push(Value::boolean(true)),
                &Instruction::False => self.stack.push(Value::boolean(false)),
                &Instruction::Nil => self.stack.push(Value::nil()),
                &Instruction::Const(addr) => {
                    let value = self.current_chunk.read_constant(addr);
                    match value {
                        Value::String(s) => self.stack.push(self.strings.intern(s).clone()),
                        _ => self.stack.push(value.clone()),
                    }
                }
            }
        }
    }

    fn peek(&'a self, distance: usize) -> &'a [Value] {
        let from = self.stack.len() - distance;
        &self.stack[from..]
    }

    #[inline]
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
