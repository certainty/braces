#[cfg(feature = "debug_vm")]
use super::disassembler::Disassembler;
use super::scheme::value;
use super::scheme::value::error;
use super::scheme::value::procedure::Procedure;
use super::scheme::value::symbol::Symbol;
use super::scheme::value::Value;
use super::stack::Stack;
use super::Error;
use super::{
    byte_code::chunk::{Chunk, LineNumber},
    scheme::value::procedure::HasArity,
};
use super::{byte_code::Instruction, scheme::value::procedure::Arity};
use super::{debug, scheme::value::foreign};
use super::{global::*, scheme::value::closure::Closure};
use crate::vm::byte_code::chunk::ConstAddressType;
use std::{
    borrow::Borrow,
    rc::{self, Rc},
};

//////////////////////////////////////////////////
// Welcome the call stack
/////////////////////////////////////////////////

// A callframe is a piece of control data
// that is associated with every live-function

#[derive(Debug)]
pub struct CallFrame {
    pub closure: Closure,
    pub ip: usize,
    pub stack_base: usize,
}

impl CallFrame {
    pub fn new(closure: Closure, stack_base: usize) -> Self {
        Self {
            ip: 0,
            stack_base,
            closure,
        }
    }

    #[inline]
    pub fn set_ip(&mut self, address: usize) {
        self.ip = address
    }

    #[inline]
    pub fn code(&self) -> &Chunk {
        self.closure.proc.code()
    }

    #[inline]
    pub fn line_number_for_current_instruction(&self) -> Option<LineNumber> {
        self.closure.proc.code().find_line(self.ip).map(|e| e.2)
    }
}

//////////////////////////////////////////////////////////////////////////////

type ValueStack = Stack<Value>;
type CallStack = Stack<CallFrame>;

pub struct Instance<'a> {
    values: &'a mut value::Factory,
    toplevel: &'a mut TopLevel,
    stack: ValueStack,
    call_stack: CallStack,
    active_frame: *mut CallFrame,
    up_values_for_next_closure: Vec<Rc<Value>>,
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
    pub fn new(
        proc: Procedure,
        call_stack_size: usize,
        toplevel: &'a mut TopLevel,
        values: &'a mut value::Factory,
    ) -> Self {
        let mut stack = ValueStack::new(call_stack_size * 255);
        let mut call_stack = CallStack::new(call_stack_size);
        let initial_closure: Closure = proc.into();

        // the first value on the stack is the initial procedure
        stack.push(Value::Closure(initial_closure.clone()));

        // the first active stack frame is that of the current procedure
        call_stack.push(CallFrame::new(initial_closure.into(), 0));

        let active_frame = call_stack.top_mut_ptr();

        Self {
            values,
            stack,
            call_stack,
            toplevel,
            active_frame,
            up_values_for_next_closure: vec![],
        }
    }

    pub fn interprete(
        proc: Procedure,
        stack_size: usize,
        toplevel: &'a mut TopLevel,
        values: &'a mut value::Factory,
    ) -> Result<Value> {
        let mut instance = Self::new(proc, stack_size, toplevel, values);
        instance.run()
    }

    fn run(&mut self) -> Result<Value> {
        #[cfg(feature = "debug_vm")]
        self.disassemble_frame();

        loop {
            #[cfg(feature = "debug_vm")]
            self.debug_cycle();

            match self.next_instruction() {
                &Instruction::Nop => (),
                &Instruction::Break => (),
                &Instruction::Pop => {
                    self.pop();
                }
                &Instruction::True => self.push(self.values.bool_true())?,
                &Instruction::False => self.push(self.values.bool_false())?,
                &Instruction::Nil => self.push(self.values.nil())?,
                &Instruction::JumpIfFalse(addr) => {
                    if self.peek(0).is_false() {
                        self.active_mut_frame().set_ip(addr)
                    }
                }
                &Instruction::Jump(addr) => self.active_mut_frame().set_ip(addr),
                &Instruction::Const(addr) => self.push(self.read_constant(addr).clone())?,
                &Instruction::Closure(addr) => self.create_closure(addr)?,
                &Instruction::UpValue(addr, is_local) => self.setup_up_value(addr, is_local)?,
                &Instruction::Return => {
                    let value = self.pop();

                    if self.pop_frame() <= 0 {
                        return Ok(value.clone());
                    } else {
                        self.push(value.clone())?
                    }
                }
                &Instruction::GetGlobal(addr) => {
                    let id = self.read_identifier(addr)?;

                    if let Some(value) = self.toplevel.get_owned(&id) {
                        self.push(value)?;
                    } else {
                        return self.runtime_error(error::undefined_variable(id));
                    }
                }
                &Instruction::GetUpValue(addr) => {
                    let value = self.active_frame().closure.get_up_value(addr);
                    self.push((*value).clone())?
                }
                &Instruction::GetLocal(addr) => {
                    self.push(self.frame_get_slot(addr).clone())?;
                }
                &Instruction::SetGlobal(addr) => self.set_global(addr)?,
                &Instruction::SetUpValue(addr) => {
                    let value = self.peek(0).clone();
                    self.active_mut_frame().closure.set_up_value(addr, value);
                }
                &Instruction::SetLocal(addr) => self.frame_set_slot(addr, self.peek(0).clone()),
                &Instruction::Define(addr) => self.define_value(addr)?,
                &Instruction::Call(args) => self.apply(args)?,
            }
        }
    }

    #[inline]
    fn next_instruction(&mut self) -> &Instruction {
        let frame = self.active_mut_frame();
        let ip = frame.ip;
        frame.ip += 1;
        frame.code().at(ip)
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
        self.stack.pop().into()
    }

    #[inline]
    fn pop_n(&mut self, n: usize) -> Vec<Value> {
        let mut result = vec![];

        for _ in 0..n {
            result.push(self.pop())
        }

        result
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
    fn push_frame(&mut self, closure: value::closure::Closure, arg_count: usize) -> Result<()> {
        let base = std::cmp::max(self.stack.len() - arg_count, 0);
        let frame = CallFrame::new(closure, base);
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
    fn frame_get_slot(&self, slot_address: ConstAddressType) -> &Value {
        let peek_distance =
            self.stack.len() - (self.active_frame().stack_base + (slot_address as usize)) - 1;
        self.peek(peek_distance)
    }

    #[inline]
    fn frame_set_slot(&mut self, slot_address: ConstAddressType, value: Value) {
        let slot_address = self.active_frame().stack_base + (slot_address as usize);
        self.stack.set(slot_address, value);
    }

    // specific VM instructions
    fn setup_up_value(&mut self, addr: ConstAddressType, is_local: bool) -> Result<()> {
        if is_local {
            let value = self.capture_up_value(addr);
            self.up_values_for_next_closure.push(value);
        } else {
            self.up_values_for_next_closure
                .push(self.active_frame().closure.get_up_value(addr));
        }
        Ok(())
    }

    fn capture_up_value(&mut self, addr: ConstAddressType) -> Rc<Value> {
        let value = self.frame_get_slot(addr);
        Rc::new(Value::UpValue(Rc::new(value.clone())))
    }

    fn create_closure(&mut self, addr: ConstAddressType) -> Result<()> {
        match self.read_constant(addr).clone() {
            Value::Procedure(proc) => {
                let up_values = self.up_values_for_next_closure.to_owned();
                let closure = Closure::new(proc, up_values);
                self.up_values_for_next_closure.truncate(0);
                self.push(Value::Closure(closure))
            }
            _ => return self.compiler_bug("Expected closure function"),
        }
    }

    #[inline]
    fn apply(&mut self, args: usize) -> Result<()> {
        match &self.peek(args) {
            &value::Value::Closure(cl) => self.apply_native(cl.proc.clone(), args),
            value::Value::Procedure(p) => self.apply_native(p.clone(), args),
            value::Value::ForeignProcedure(p) => self.apply_foreign(p.clone(), args),
            &other => return self.runtime_error(error::non_callable(other.clone())),
        }
    }

    #[inline]
    fn apply_native(&mut self, proc: std::rc::Rc<Procedure>, arg_count: usize) -> Result<()> {
        self.check_arity(&proc, arg_count)?;
        let arg_count = self.bind_arguments(&proc, arg_count)?;
        self.push_frame(proc.into(), arg_count)?;
        #[cfg(feature = "debug_vm")]
        self.disassemble_frame();
        Ok(())
    }

    #[inline]
    fn apply_foreign(
        &mut self,
        proc: std::rc::Rc<foreign::Procedure>,
        arg_count: usize,
    ) -> Result<()> {
        self.check_arity(&proc, arg_count)?;
        let arguments = self.pop_n(arg_count).iter().cloned().collect();
        match proc.call(arguments) {
            Ok(v) => {
                self.push(v)?;
                Ok(())
            }
            Err(e) => self.runtime_error(e),
        }
    }

    fn bind_arguments<T: HasArity>(&mut self, proc: &T, arg_count: usize) -> Result<usize> {
        match proc.arity() {
            Arity::Exactly(_) => Ok(arg_count), // nothing to do as the variables are layed out as expected already on the stack
            Arity::AtLeast(n) => {
                // stuff the last values into a new local
                let rest_count = arg_count - n;
                let mut rest_values = self.pop_n(rest_count);
                rest_values.reverse();
                let rest_list = self.values.proper_list(rest_values);
                self.push(rest_list)?;
                Ok(n + 1)
            }
            Arity::Many => {
                let mut rest_values = self.pop_n(arg_count);
                rest_values.reverse();
                let rest_list = self.values.proper_list(rest_values);
                self.push(rest_list)?;
                Ok(1)
            }
        }
    }

    fn check_arity<T: HasArity>(&self, proc: &T, arg_count: usize) -> Result<()> {
        match proc.arity() {
            Arity::Exactly(n) if arg_count == *n => Ok(()),
            Arity::AtLeast(n) if arg_count >= *n => Ok(()),
            Arity::Many => Ok(()),
            other => self.runtime_error(error::arity_mismatch(other.clone(), arg_count)),
        }
    }

    fn define_value(&mut self, addr: ConstAddressType) -> Result<()> {
        let v = self.pop();
        let id = self.read_identifier(addr)?;
        self.toplevel.set(id.clone(), v.clone());
        self.push(self.values.unspecified())?;
        Ok(())
    }

    fn set_global(&mut self, addr: ConstAddressType) -> Result<()> {
        let v = self.pop();
        let id = self.read_identifier(addr)?;

        if !self.toplevel.get(&id).is_some() {
            return self.runtime_error(error::undefined_variable(id.clone()));
        } else {
            self.toplevel.set(id.clone(), v.clone());
            self.push(self.values.unspecified())?;
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

    // TODO: add a representation for stack trace and add it to the error
    fn runtime_error<T>(&self, e: error::RuntimeError) -> Result<T> {
        let result = Err(Error::RuntimeError(
            e,
            self.active_frame()
                .line_number_for_current_instruction()
                .unwrap_or(0),
        ));
        result
    }

    // Debug the VM

    #[cfg(feature = "debug_vm")]
    fn debug_cycle(&mut self) {
        let mut disassembler = Disassembler::new(std::io::stdout());
        let chunk = self.active_frame().code();

        println!("{}", debug::stack::pretty_print(&self.stack));
        disassembler.disassemble_instruction(chunk, self.active_frame().ip);
    }

    #[cfg(feature = "debug_vm")]
    fn disassemble_frame(&mut self) {
        let mut disassembler = Disassembler::new(std::io::stdout());
        let base_addr = self.active_frame().stack_base;
        let chunk = self.active_frame().code();
        let proc_name = self.active_frame().closure.proc.name();
        println!("\n");
        disassembler.disassemble(
            chunk,
            &format!(
                "ACTIVE FRAME (proc: {}, stack_base: {})",
                proc_name, base_addr
            ),
        );
        println!("\n");
    }
}
