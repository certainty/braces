use super::byte_code::chunk::{AddressType, Chunk};
use super::byte_code::Instruction;
#[cfg(feature = "debug_vm")]
use super::disassembler::Disassembler;
use super::scheme::value;
use super::scheme::value::{Symbol, Value};
use super::Error;
use crate::vm::byte_code::chunk::ConstAddressType;
use crate::vm::byte_code::chunk::LineNumber;
use rustc_hash::FxHashMap;

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct TopLevel {
    bindings: FxHashMap<Symbol, Value>,
}

impl TopLevel {
    pub fn new() -> Self {
        Self {
            bindings: FxHashMap::default(),
        }
    }

    pub fn set(&mut self, k: Symbol, v: Value) {
        self.bindings.insert(k, v);
    }

    pub fn get(&self, k: &Symbol) -> Option<&Value> {
        self.bindings.get(k)
    }
}

pub struct Instance<'a> {
    ip: AddressType,
    current_chunk: &'a Chunk,
    values: &'a mut value::Factory,
    toplevel: &'a mut TopLevel,
    // could be tweaked to store references or owned values
    stack: Vec<Value>,
    #[cfg(feature = "debug_vm")]
    disassembler: Disassembler<std::io::Stdout>,
}

impl<'a> Instance<'a> {
    pub fn interprete<'b>(
        chunk: &Chunk,
        stack_size: usize,
        toplevel: &mut TopLevel,
        values: &mut value::Factory,
    ) -> Result<Value> {
        Instance {
            stack: Vec::with_capacity(stack_size),
            toplevel,
            current_chunk: &chunk,
            values: values,
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
                &Instruction::Halt => {
                    return Ok(self
                        .stack
                        .pop()
                        .map(|e| e.to_owned())
                        .unwrap_or(Value::Unspecified))
                }
                &Instruction::True => self.push(self.values.bool_true().clone()),
                &Instruction::False => self.push(self.values.bool_false().clone()),
                &Instruction::Nil => self.push(self.values.nil().clone()),
                &Instruction::Get(addr) => {
                    let id = self.read_identifier(addr)?;
                    if let Some(value) = self.toplevel.get(&id) {
                        let cloned = value.clone();
                        self.push(cloned);
                    } else {
                        return self.runtime_error(&format!("Variable {} is unbound", id.as_str()));
                    }
                }
                &Instruction::Set(addr) => {
                    if let Some(v) = self.pop() {
                        let id = self.read_identifier(addr)?;
                        self.toplevel.set(id.clone(), v.to_owned());
                        self.push(self.values.unspecified().clone());
                    } else {
                        return self.compiler_bug(&format!("Expected symbol at address: {}", addr));
                    }
                }
                &Instruction::Const(addr) => {
                    let value = self.current_chunk.read_constant(addr);
                    match value {
                        Value::UninternedString(s) => {
                            let interned = self.values.interned_string(s);
                            self.push(interned)
                        }
                        _ => self.push(value.clone()),
                    }
                }
            }
        }
    }

    fn read_identifier(&self, addr: ConstAddressType) -> Result<Symbol> {
        if let Value::Symbol(s) = self.current_chunk.read_constant(addr) {
            Ok(s.clone())
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

    fn push(&mut self, v: Value) {
        self.stack.push(v)
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
