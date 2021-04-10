use super::byte_code::chunk::{AddressType, Chunk};
use super::byte_code::Instruction;
#[cfg(feature = "debug_vm")]
use super::disassembler::Disassembler;
use super::scheme::value::Value;
use super::Error;
use crate::vm::byte_code::chunk::ConstAddressType;
use crate::vm::byte_code::chunk::LineNumber;
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

struct TopLevel {
    bindings: FxHashMap<String, Value>,
}

impl TopLevel {
    pub fn new() -> Self {
        Self {
            bindings: FxHashMap::default(),
        }
    }

    pub fn set(&mut self, k: String, v: Value) {
        self.bindings.insert(k, v);
    }

    pub fn get(&self, k: &String) -> Option<&Value> {
        self.bindings.get(k)
    }
}

pub struct Instance<'a> {
    ip: AddressType,
    current_chunk: &'a Chunk,
    strings: StringTable,
    toplevel: TopLevel,
    stack: Vec<Value>,
    #[cfg(feature = "debug_vm")]
    disassembler: Disassembler<std::io::Stdout>,
}

impl<'a> Instance<'a> {
    pub fn interprete(chunk: &Chunk, stack_size: usize) -> Result<Value> {
        Instance {
            stack: Vec::with_capacity(stack_size),
            toplevel: TopLevel::new(),
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
                &Instruction::Get(addr) => {
                    let id = self.read_identifier(addr)?;
                    if let Some(value) = self.toplevel.get(id) {
                        self.stack.push(value.clone());
                    } else {
                        return self.runtime_error(&format!("Variable {} is unbound", id));
                    }
                }
                &Instruction::Set(addr) => {
                    let value = self.pop();

                    if let Some(v) = value {
                        let id = self.read_identifier(addr)?.clone();
                        self.toplevel.set(id, v);
                    } else {
                        return self.compiler_bug(&format!("Expected symbol at address: {}", addr));
                    }
                }
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

    fn read_identifier(&self, addr: ConstAddressType) -> Result<&String> {
        if let Value::Symbol(s) = self.current_chunk.read_constant(addr) {
            Ok(s)
        } else {
            self.compiler_bug(&format!("Expected symbol at address: {}", addr))
        }
    }

    fn compiler_bug<T>(&self, message: &str) -> Result<T> {
        Err(Error::CompilerBug(message.to_string()))
    }

    fn runtime_error<T>(&self, message: &str) -> Result<T> {
        Err(Error::RuntimeError(
            message.to_string(),
            self.line_number_for_current_instruction().unwrap_or(0),
        ))
    }

    fn line_number_for_current_instruction(&self) -> Option<LineNumber> {
        self.current_chunk.find_line(self.ip - 1).map(|e| e.2)
    }

    fn peek(&'a self, distance: usize) -> &'a [Value] {
        let from = self.stack.len() - distance;
        &self.stack[from..]
    }

    fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
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
