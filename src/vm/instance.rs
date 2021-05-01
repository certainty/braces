use super::byte_code::Instruction;
use super::call_frame::*;
#[cfg(feature = "debug_vm")]
use super::disassembler::Disassembler;
use super::global::*;
use super::scheme::value;
use super::scheme::value::{Symbol, Value};
use super::Error;
use crate::vm::byte_code::chunk::{AddressType, ConstAddressType, LineNumber};
use arrayvec::ArrayVec;

const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = FRAMES_MAX * 256;

type Result<T> = std::result::Result<T, Error>;

pub struct Instance<'a> {
    values: &'a mut value::Factory,
    toplevel: &'a mut TopLevel,
    frames: ArrayVec<CallFrame, FRAMES_MAX>,
    frame: *mut CallFrame,
    stack: ArrayVec<Value, STACK_MAX>,
    stack_top: *mut Value,
}

// TODO:
// The vm isn't optimised for performance yet.
// There are several things that could be more efficient using unsafe pointer code
// Most notably access to the stack and the callstack (including ip increments)
impl<'a> Instance<'a> {
    pub fn interprete<'b>(
        proc: value::lambda::Procedure,
        _stack_size: usize,
        toplevel: &mut TopLevel,
        values: &mut value::Factory,
    ) -> Result<Value> {
        let mut stack = ArrayVec::<_, STACK_MAX>::new();
        let mut frames = ArrayVec::<_, FRAMES_MAX>::new();
        let frame = frames.as_mut_ptr();
        let stack_top = stack.as_mut_ptr();
        frames.push(CallFrame::new(stack.as_mut_ptr(), proc));

        Instance {
            frames,
            frame,
            stack,
            stack_top,
            toplevel,
            values,
        }
        .run()
    }

    fn run(&mut self) -> Result<Value> {
        //#[cfg(feature = "debug_vm")]
        //self.disassemble_frame();
        loop {
            #[cfg(feature = "debug_vm")]
            self.debug_cycle();

            match self.next_instruction() {
                &Instruction::Nop => (),
                &Instruction::Break => (),
                &Instruction::Pop => {
                    self.pop();
                }
                &Instruction::True => self.push(self.values.bool_true().clone())?,
                &Instruction::False => self.push(self.values.bool_false().clone())?,
                &Instruction::Nil => self.push(self.values.nil().clone())?,
                &Instruction::Const(addr) => self.push(self.read_constant(addr).clone())?,
                &Instruction::Return => {
                    let value = self.pop();
                    if self.pop_frame() <= 0 {
                        return Ok(value);
                    } else {
                        self.push(value)?
                    }
                }
                &Instruction::Get(addr) => {
                    let id = self.read_identifier(addr)?;

                    if let Some(value) = self.toplevel.get(&id) {
                        self.push(value.clone())?;
                    } else {
                        return self.runtime_error(&format!("Variable {} is unbound", id.as_str()));
                    }
                }
                &Instruction::GetLocal(addr) => {
                    let value = unsafe { (*self.frame).get_slot(addr) };
                    self.push(value.clone())?
                }
                &Instruction::SetLocal(addr) => unsafe {
                    (*self.frame).set_slot(addr, self.peek(0).clone());
                },
                &Instruction::Set(addr) => self.set_value(addr)?,
                &Instruction::Define(addr) => self.define_value(addr)?,
                &Instruction::Call(args) => self.apply(args)?,
            }
        }
    }

    #[inline]
    fn next_instruction(&mut self) -> &Instruction {
        unsafe {
            let inst = &*(*self.frame).ip;
            (*self.frame).ip = (*self.frame).ip.offset(1);
            inst
        }
    }

    #[inline]
    fn read_constant(&self, addr: ConstAddressType) -> &Value {
        self.current_frame().code().read_constant(addr)
    }

    //
    // Stack operations ->
    //
    #[inline]
    fn stack_reset(&mut self) -> Result<()> {
        self.stack.truncate(0);
        self.stack_top = self.stack.as_mut_ptr();

        self.frames.truncate(0);
        self.frame = self.frames.as_mut_ptr();
        Ok(())
    }

    #[inline]
    fn push(&mut self, v: Value) -> Result<()> {
        println!("Pushing {:?}", v);
        self.stack.push(v);
        unsafe { self.stack_top = self.stack_top.add(1) };
        println!("Stack ptr: {:?} Value: {:?}", self.stack_top, unsafe {
            &*self.stack_top
        });
        Ok(())
    }

    #[inline]
    fn pop(&mut self) -> Value {
        let value = self.stack.pop().unwrap();
        unsafe { self.stack_top = self.stack_top.sub(1) };
        value
    }

    #[inline]
    fn peek(&self, distance: isize) -> &Value {
        //unsafe { &(*self.stack_top.sub(distance as usize)) }
        &self.stack[self.stack.len() - (distance as usize) - 1]
    }
    //
    // <- Stack operations
    //

    //
    // Call stack operations
    //

    #[inline]
    fn pop_frame(&mut self) -> usize {
        self.frames.pop();
        unsafe { self.frame = self.frame.offset(-1) }
        self.frames.len()
    }

    #[inline]
    fn push_frame(&mut self, proc: value::lambda::Procedure, arg_count: isize) -> Result<()> {
        let slots = unsafe { self.stack_top.sub(arg_count as usize) };
        let frame = CallFrame::new(slots, proc);
        self.frames.push(frame);
        unsafe { self.frame = self.frame.offset(1) };
        Ok(())
    }

    #[inline]
    fn current_frame(&self) -> &CallFrame {
        unsafe { &(*self.frame) }
    }

    #[inline]
    fn apply(&mut self, args: isize) -> Result<()> {
        println!("Reading procedure from: {}", args);
        match self.peek(args) {
            value::Value::Procedure(proc) => {
                // super expensive to clone the chunk
                println!("DONE!");
                self.push_frame(proc.clone(), args)
            }
            other => {
                return self
                    .runtime_error(&format!("Operator is not a callable object: {:?}", other))
            }
        }
    }

    fn define_value(&mut self, addr: ConstAddressType) -> Result<()> {
        let v = self.pop();
        let id = self.read_identifier(addr)?;
        self.toplevel.set(id.clone(), v.to_owned());
        self.push(self.values.unspecified().clone())?;
        Ok(())
    }

    fn set_value(&mut self, addr: ConstAddressType) -> Result<()> {
        let v = self.pop();
        let id = self.read_identifier(addr)?;

        if !self.toplevel.get(&id).is_some() {
            return self.runtime_error(&format!(
                "Can't set! {} because it's undefined",
                id.as_str()
            ));
        } else {
            self.toplevel.set(id.clone(), v.to_owned());
            self.push(self.values.unspecified().clone())?;
        }

        Ok(())
    }

    fn read_identifier(&mut self, addr: ConstAddressType) -> Result<Symbol> {
        if let Value::Symbol(s) = self.read_constant(addr) {
            Ok(s.clone())
        } else {
            self.compiler_bug(&format!("Expected symbol at address: {}", addr))
        }
    }

    #[inline]
    fn compiler_bug<T>(&mut self, message: &str) -> Result<T> {
        let result = Err(Error::CompilerBug(message.to_string()));
        self.stack_reset()?;
        result
    }

    fn runtime_error<T>(&mut self, message: &str) -> Result<T> {
        let result = Err(Error::RuntimeError(
            message.to_string(),
            self.line_number_for_current_instruction().unwrap_or(0),
        ));

        self.stack_reset()?;
        result
    }

    #[inline]
    fn line_number_for_current_instruction(&mut self) -> Option<LineNumber> {
        let address = self.ip_address();
        self.current_frame().code().find_line(address).map(|e| e.2)
    }

    #[inline]
    fn ip_address(&self) -> AddressType {
        let base = self.current_frame().code().as_ptr() as usize;
        let ip = self.current_frame().ip as usize;
        ip - base
    }

    #[cfg(feature = "debug_vm")]
    fn debug_cycle(&mut self) {
        let mut disassembler = Disassembler::new(std::io::stdout());
        let chunk = unsafe { (*self.frame).code() };

        self.print_stack();
        //disassembler.disassemble_instruction(chunk, self.ip_address());
    }

    // print the stack better
    fn print_stack(&self) {
        println!("==== Stack ====\n");
        print!("     ");
        let mut longest_value = 0;
        let mut values: Vec<String> = vec![];

        for value in self.stack.iter().rev() {
            let v = format!("{:?}", value);
            longest_value = std::cmp::max(longest_value, v.len());
            values.push(v);
        }

        for current in values {
            print!("[{:width$}]", current, width = longest_value);
        }

        println!("\n");
    }

    #[cfg(feature = "debug_vm")]
    fn disassemble_frame(&mut self) {
        let mut disassembler = Disassembler::new(std::io::stdout());
        let chunk = self.current_frame().code();
        disassembler.disassemble(chunk, "FRAME");
    }
}
