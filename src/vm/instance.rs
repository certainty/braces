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

#[derive(Debug)]
pub struct CallFrame {
    proc: value::lambda::Procedure,
    ip: AddressType,
    base: usize,
}

impl CallFrame {
    pub fn new(base: usize, proc: value::lambda::Procedure) -> Self {
        CallFrame {
            base: base,
            ip: 0,
            proc: proc,
        }
    }

    pub fn code(&self) -> &Chunk {
        self.proc.code()
    }
}

pub struct Instance<'a> {
    values: &'a mut value::Factory,
    toplevel: &'a mut TopLevel,
    frames: Vec<CallFrame>,
    // could be tweaked to store references or owned values
    stack: Vec<Value>,
}

// TODO:
// The vm isn't optimised for performance yet.
// There are several things that could be more efficient using unsafe pointer code
// Most notably access to the stack and the callstack (including ip increments)
impl<'a> Instance<'a> {
    pub fn interprete<'b>(
        proc: value::lambda::Procedure,
        stack_size: usize,
        toplevel: &mut TopLevel,
        values: &mut value::Factory,
    ) -> Result<Value> {
        Instance {
            frames: vec![CallFrame::new(0, proc)],
            stack: Vec::with_capacity(stack_size),
            toplevel,
            values: values,
        }
        .run()
    }

    fn run(&mut self) -> Result<Value> {
        #[cfg(feature = "debug_vm")]
        self.disassemble_frame();

        loop {
            #[cfg(feature = "debug_vm")]
            self.debug_cycle();
            std::thread::sleep_ms(2000);

            match self.next_instruction() {
                &Instruction::Return => {
                    return Ok(self
                        .stack
                        .pop()
                        .map(|e| e.to_owned())
                        .unwrap_or(Value::Unspecified))
                }
                &Instruction::Nop => (),
                &Instruction::Pop => {
                    self.pop();
                }
                &Instruction::Call(args) => match &self.peek(args) {
                    &Some(value::Value::Procedure(proc)) => {
                        self.apply(proc.clone(), args)?;
                    }
                    _ => {
                        return self.runtime_error(&format!("Operator is not a callable object"));
                    }
                },
                &Instruction::Break => (),
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
                &Instruction::GetLocal(addr) => {
                    let base = self.current_frame().base;
                    let slot = self.stack[base + (addr as usize)].clone();
                    self.push(slot)
                }
                &Instruction::SetLocal(addr) => {
                    let base = self.current_frame().base;
                    if let Some(v) = self.peek(0) {
                        self.stack[base + (addr as usize)] = v.clone();
                    } else {
                        return self.compiler_bug(&format!(
                            "Couldn't set local variable on address: {}",
                            addr
                        ));
                    }
                }
                &Instruction::Set(addr) => {
                    self.set_value(addr)?;
                }
                &Instruction::Define(addr) => {
                    self.define_value(addr)?;
                }
                &Instruction::Const(addr) => {
                    let chunk = self.current_frame().code();
                    let value = chunk.read_constant(addr);
                    match value {
                        _ => self.push(value.clone()),
                    }
                }
            }
        }
    }

    fn apply(&mut self, proc: value::lambda::Procedure, arg_count: usize) -> Result<()> {
        let base = self.stack.len() - 1 - arg_count - 1;
        self.frames.push(CallFrame::new(base, proc));
        Ok(())
    }

    fn define_value(&mut self, addr: ConstAddressType) -> Result<()> {
        if let Some(v) = self.pop() {
            let id = self.read_identifier(addr)?;
            self.toplevel.set(id.clone(), v.to_owned());
            self.push(self.values.unspecified().clone());
        } else {
            return self.compiler_bug(&format!("Expected symbol at address: {}", addr));
        }

        Ok(())
    }

    fn set_value(&mut self, addr: ConstAddressType) -> Result<()> {
        if let Some(v) = self.pop() {
            let id = self.read_identifier(addr)?;
            if !self.toplevel.get(&id).is_some() {
                return self.runtime_error(&format!(
                    "Can't set! {} because it's undefined",
                    id.as_str()
                ));
            } else {
                self.toplevel.set(id.clone(), v.to_owned());
                self.push(self.values.unspecified().clone());
            }
        } else {
            return self.compiler_bug(&format!("Expected symbol at address: {}", addr));
        }

        Ok(())
    }

    fn read_identifier(&mut self, addr: ConstAddressType) -> Result<Symbol> {
        let chunk = self.current_frame().code();

        if let Value::Symbol(s) = chunk.read_constant(addr) {
            Ok(s.clone())
        } else {
            self.compiler_bug(&format!("Expected symbol at address: {}", addr))
        }
    }

    #[inline]
    fn compiler_bug<T>(&self, message: &str) -> Result<T> {
        Err(Error::CompilerBug(message.to_string()))
    }

    fn runtime_error<T>(&mut self, message: &str) -> Result<T> {
        Err(Error::RuntimeError(
            message.to_string(),
            self.line_number_for_current_instruction().unwrap_or(0),
        ))
    }

    #[inline]
    fn line_number_for_current_instruction(&mut self) -> Option<LineNumber> {
        let frame = self.current_frame();
        frame.code().find_line(frame.ip - 1).map(|e| e.2)
    }

    #[inline]
    fn push(&mut self, v: Value) {
        self.stack.push(v)
    }

    #[inline]
    fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }

    #[inline]
    fn peek(&self, slot: usize) -> Option<&Value> {
        self.stack.get(self.stack.len() - slot - 1)
    }

    #[inline]
    fn next_instruction(&mut self) -> &Instruction {
        let mut frame = self.current_mut_frame();
        let instruction = frame.proc.code().read_instruction(frame.ip);
        frame.ip = frame.ip + 1;
        instruction
    }

    // TODO: replace with faster variant that returns a mut ptr
    #[inline]
    fn current_mut_frame(&mut self) -> &mut CallFrame {
        let index = self.frames.len() - 1;
        &mut self.frames[index]
    }

    #[inline]
    fn current_frame(&self) -> &CallFrame {
        let index = self.frames.len() - 1;
        &self.frames[index]
    }

    #[cfg(feature = "debug_vm")]
    fn debug_cycle(&mut self) {
        let mut disassembler = Disassembler::new(std::io::stdout());
        let frame = self.current_frame();
        let chunk = self.current_frame().code();

        self.print_stack();
        disassembler.disassemble_instruction(chunk, frame.ip);
    }

    #[cfg(feature = "debug_vm")]
    fn print_stack(&self) {
        print!("     ");
        for value in self.stack.iter() {
            print!("[{:?}]", value);
        }
        println!("")
    }

    #[cfg(feature = "debug_vm")]
    fn disassemble_frame(&self) {
        let mut disassembler = Disassembler::new(std::io::stdout());
        let chunk = self.current_frame().code();
        disassembler.disassemble(chunk, "DEBUG DISASS");
    }
}
