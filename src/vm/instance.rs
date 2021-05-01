use super::byte_code::Instruction;
use super::call_frame::*;
#[cfg(feature = "debug_vm")]
use super::disassembler::Disassembler;
use super::global::*;
use super::scheme::value;
use super::scheme::value::{Symbol, Value};
use super::stack;
use super::stack::Stack;
use super::Error;
use crate::vm::byte_code::chunk::ConstAddressType;
use std::rc::Rc;

const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = FRAMES_MAX * 256;

type Result<T> = std::result::Result<T, Error>;

type ValueStack = Stack<Value>;
type ValueFrame = stack::Frame<Value>;
type CallStack = Stack<CallFrame>;

pub struct Instance<'a> {
    values: &'a mut value::Factory,
    toplevel: &'a mut TopLevel,
    stack: ValueStack,
    call_stack: CallStack,
    active_frame: *mut CallFrame,
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
        let mut stack = ValueStack::new(stack_size);
        let mut call_stack = CallStack::new(FRAMES_MAX);
        call_stack.push(CallFrame::new(
            stack::Frame::from(stack.as_mut_ptr(), 0),
            Rc::new(proc),
        ));
        let active_frame = call_stack.top_mut_ptr();

        let mut instance = Instance {
            values,
            stack,
            call_stack,
            toplevel,
            active_frame,
        };

        instance.run()
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
                    let value = self.active_frame().get_slot(addr);
                    self.push(value.clone())?
                }
                &Instruction::SetLocal(addr) => {
                    self.active_mut_frame().set_slot(addr, self.peek(0).clone())
                }
                &Instruction::Set(addr) => self.set_value(addr)?,
                &Instruction::Define(addr) => self.define_value(addr)?,
                &Instruction::Call(args) => self.apply(args)?,
            }
        }
    }

    #[inline]
    fn next_instruction(&mut self) -> &Instruction {
        self.active_mut_frame().next_instruction()
    }

    #[inline]
    fn read_constant(&self, addr: ConstAddressType) -> &Value {
        self.active_frame().code().read_constant(addr)
    }

    //
    // Stack operations ->
    //
    #[inline]
    fn stack_reset(&mut self) -> Result<()> {
        // TODO: implement in stack
        Ok(())
    }

    #[inline]
    fn push(&mut self, v: Value) -> Result<()> {
        self.stack.push(v);
        Ok(())
    }

    #[inline]
    fn pop(&mut self) -> Value {
        self.stack.pop()
    }

    #[inline]
    fn peek(&self, distance: usize) -> &Value {
        self.stack.peek(distance)
    }
    //
    // <- Stack operations
    //

    //
    // Call stack operations
    //

    #[inline]
    fn pop_frame(&mut self) -> usize {
        self.call_stack.pop();
        self.active_frame = self.call_stack.top_mut_ptr();
        self.call_stack.len()
    }

    #[inline]
    fn push_frame(&mut self, proc: Rc<value::lambda::Procedure>, arg_count: usize) -> Result<()> {
        let base = self.stack.len() - arg_count;
        let frame = CallFrame::new(stack::Frame::from(self.stack.as_mut_ptr(), base), proc);
        self.call_stack.push(frame);
        self.active_frame = self.call_stack.top_mut_ptr();
        Ok(())
    }

    #[inline]
    fn active_frame(&self) -> &CallFrame {
        unsafe { &(*self.active_frame) }
    }

    #[inline]
    fn active_mut_frame(&self) -> &mut CallFrame {
        unsafe { &mut (*self.active_frame) }
    }

    #[inline]
    fn apply(&mut self, args: usize) -> Result<()> {
        let is_callable = match self.peek(0) {
            value::Value::Procedure(proc) => true,
            _ => false,
        };

        if !is_callable {
            return self.runtime_error(&format!("Operator is not a callable object"));
        }

        if let value::Value::Procedure(proc) = self.peek(0) {
            self.push_frame(proc.clone(), args)?;
        }

        Ok(())
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

    fn runtime_error<T>(&self, message: &str) -> Result<T> {
        let result = Err(Error::RuntimeError(
            message.to_string(),
            self.active_frame()
                .line_number_for_current_instruction()
                .unwrap_or(0),
        ));
        result
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

        for value in self.stack.as_vec().iter().rev() {
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
