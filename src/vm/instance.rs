use super::byte_code::Instruction;
use super::call_frame::*;
use super::debug;
#[cfg(feature = "debug_vm")]
use super::disassembler::Disassembler;
use super::global::*;
use super::scheme::value;
use super::scheme::value::lambda::Procedure;
use super::scheme::value::{Symbol, Value};
use super::stack;
use super::stack::Stack;
use super::Error;
use crate::vm::byte_code::chunk::ConstAddressType;
use std::rc::Rc;

//////////////////////////////////////////////////
// Welcome the call stack
/////////////////////////////////////////////////

// A callframe is a piece of control data
// that is associated with every live-function
//
//
//pub struct CallFrame {
//    function: Rc<Procedure>, // the function that is currently executed
//}

const FRAMES_MAX: usize = 64;
type ValueStack = Stack<Value>;
type CallStack = Stack<CallFrame>;

pub struct Instance<'a> {
    values: &'a mut value::Factory,
    toplevel: &'a mut TopLevel,
    stack: ValueStack,
    call_stack: CallStack,
    active_frame: *mut CallFrame,
}

////////////////////////////////////////////////////////
// VM Implementation
///////////////////////////////////////////////////////

type Result<T> = std::result::Result<T, Error>;
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
        let proc = Rc::new(proc);

        stack.push(Value::Procedure(proc.clone()));
        call_stack.push(CallFrame::new(
            stack::Frame::from(stack.as_mut_ptr(), 0),
            proc.clone(),
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

                    if let Some(value) = self.toplevel.get_owned(&id) {
                        self.push(value)?;
                    } else {
                        return self.runtime_error(&format!("Variable {} is unbound", id.as_str()));
                    }
                }
                &Instruction::GetLocal(addr) => {
                    println!("Get Local {:?}", self.active_frame());
                    let value = self.active_frame().get_slot(addr).clone();
                    self.push(value)?
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
        let len = self.call_stack.len();
        if len > 0 {
            self.active_frame = self.call_stack.top_mut_ptr();
        }
        len
    }

    #[inline]
    fn push_frame(&mut self, proc: Rc<value::lambda::Procedure>, arg_count: usize) -> Result<()> {
        let base = std::cmp::max(self.stack.len() - arg_count - 1, 0);
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
        if let value::Value::Procedure(proc) = self.peek(args).clone() {
            self.push_frame(proc.clone(), args)?;

            #[cfg(feature = "debug_vm")]
            self.disassemble_frame();
        } else {
            return self.runtime_error(&format!("Operator is not a callable object"));
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
        let chunk = self.active_frame().code();

        self.print_stack();
        disassembler.disassemble_instruction(chunk, self.active_frame().ip);
    }

    // print the stack better
    fn print_stack(&self) {
        println!("{}", debug::stack::pretty_print(&self.stack));
    }

    #[cfg(feature = "debug_vm")]
    fn disassemble_frame(&mut self) {
        let mut disassembler = Disassembler::new(std::io::stdout());
        let chunk = self.active_frame().code();
        println!("\n");
        disassembler.disassemble(chunk, "ACTIVE FRAME");
        println!("\n");
    }
}
