pub mod call_frame;
use crate::vm::byte_code::chunk::AddressType;
use rustc_hash::FxHashMap;

use super::byte_code::Instruction;
#[cfg(feature = "debug_vm")]
use super::debug;
use super::disassembler::Disassembler;
use super::global::*;
use super::stack::Stack;
use super::value;
use super::value::closure::Closure;
use super::value::error;
use super::value::procedure::{self, Arity};
use super::value::symbol::Symbol;
use super::value::Value;
use super::Error;
use crate::vm::byte_code::chunk::ConstAddressType;
use crate::vm::value::RefValue;
use call_frame::CallFrame;
use std::rc::Rc;

type Result<T> = std::result::Result<T, Error>;

type ValueStack = Stack<Value>;
type CallStack = Stack<CallFrame>;

pub struct Instance<'a> {
    // the value factory which can be shared between individual instance runs
    values: &'a mut value::Factory,
    // top level environment which can be shared between individual instance runs
    toplevel: &'a mut TopLevel,
    // a simple stack to manage intermediate values and locals
    stack: ValueStack,
    // manage all live functions
    call_stack: CallStack,
    // the currently active stack frame
    active_frame: *mut CallFrame,
    // open up-values are indexed by absolute stack address
    open_up_values: FxHashMap<AddressType, RefValue>,
}

// TODO: Optimize for performance
// Likely candidates for optimizations are the stack(s)
impl<'a> Instance<'a> {
    pub fn new(
        initial_closure: value::closure::Closure,
        call_stack_size: usize,
        toplevel: &'a mut TopLevel,
        values: &'a mut value::Factory,
    ) -> Self {
        let mut stack = ValueStack::new(call_stack_size * 255);
        let mut call_stack = CallStack::new(call_stack_size);

        // the first value on the stack is the initial procedure
        stack.push(Value::Closure(initial_closure.clone()));

        // the first active stack frame is that of the current procedure
        call_stack.push(CallFrame::new(initial_closure, 0));

        let active_frame = call_stack.top_mut_ptr();

        Self {
            values,
            stack,
            call_stack,
            toplevel,
            active_frame,
            open_up_values: FxHashMap::default(),
        }
    }

    pub fn interprete(
        initial_closure: value::closure::Closure,
        stack_size: usize,
        toplevel: &'a mut TopLevel,
        values: &'a mut value::Factory,
    ) -> Result<Value> {
        let mut instance = Self::new(initial_closure, stack_size, toplevel, values);
        instance.run()
    }

    fn run(&mut self) -> Result<Value> {
        #[cfg(feature = "debug_vm")]
        self.disassemble_frame();

        loop {
            #[cfg(feature = "debug_vm")]
            self.debug_cycle();

            match self.next_instruction() {
                &Instruction::True => self.push(self.values.bool_true())?,
                &Instruction::False => self.push(self.values.bool_false())?,
                &Instruction::Nil => self.push(self.values.nil())?,
                &Instruction::Const(addr) => self.push(self.read_constant(addr).clone())?,

                &Instruction::Define(addr) => self.define_value(addr)?,

                &Instruction::GetGlobal(addr) => self.get_global(addr)?,
                &Instruction::SetGlobal(addr) => self.set_global(addr)?,
                &Instruction::UpValue(addr, is_local) => self.create_up_value(addr, is_local)?,
                &Instruction::CloseUpValue(addr) => self.close_up_value(addr)?,
                &Instruction::GetUpValue(addr) => self.get_up_value(addr)?,
                &Instruction::SetUpValue(addr) => self.set_up_value(addr)?,
                &Instruction::GetLocal(addr) => self.get_local(addr)?,
                &Instruction::SetLocal(addr) => self.set_local(addr)?,

                &Instruction::Closure(addr) => self.create_closure(addr)?,

                &Instruction::Call(args) => self.apply(args)?,

                &Instruction::JumpIfFalse(to) => self.jump_if_false(to)?,
                &Instruction::Jump(to) => self.jump(to)?,
                &Instruction::Return => {
                    if let Some(value) = self._return()? {
                        return Ok(value);
                    }
                }
                &Instruction::Nop => (),   // do nothing
                &Instruction::Break => (), // reserved for future use in a debugger
                &Instruction::Pop => {
                    self.pop();
                }
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

    ///////////////////////////////////////////////////////////
    //
    // Manage the value stack
    //
    ///////////////////////////////////////////////////////////

    #[inline]
    fn stack_reset(&mut self) -> Result<()> {
        // TODO: do we need this?
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

    // Return the item that is `distance` slots away from the top of the stack.
    //
    // Let the following be the stack:
    //
    //      ┌───────────────┐
    // 03   │     0x1       │
    //      ├───────────────┤
    // 02   │     0x5       │
    //      ├───────────────┤
    // 01   │     0x10      │
    //      └───────────────┘
    //
    // peek(0) returns 0x1
    // peek(2) returns 0x10

    #[inline]
    fn peek(&self, distance: usize) -> &Value {
        self.stack.peek(distance)
    }

    ///////////////////////////////////////////////////////////
    //
    // Manage the call stack and access frame local variables
    //
    ///////////////////////////////////////////////////////////

    #[inline]
    fn push_frame(&mut self, closure: value::closure::Closure, arg_count: usize) -> Result<()> {
        let base = std::cmp::max(self.stack.len() - arg_count - 1, 0);
        let frame = CallFrame::new(closure, base);
        self.call_stack.push(frame);
        self.active_frame = self.call_stack.top_mut_ptr();
        Ok(())
    }

    #[inline]
    fn pop_frame(&mut self) -> (usize, CallFrame) {
        let frame = self.call_stack.pop();
        let len = self.call_stack.len();
        if len > 0 {
            self.active_frame = self.call_stack.top_mut_ptr();
        }
        (len, frame)
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
    fn frame_get_slot(&self, slot_address: AddressType) -> &Value {
        let index = self.frame_slot_address_to_stack_index(slot_address);
        self.stack.at(index)
    }

    #[inline]
    fn frame_slot_address_to_stack_index(&self, slot_address: AddressType) -> usize {
        self.active_frame().stack_base + (slot_address as usize)
    }

    #[inline]
    fn frame_set_slot(&mut self, slot_address: AddressType, value: Value) {
        let index = self.frame_slot_address_to_stack_index(slot_address);
        self.stack.set(index, value);
    }

    ///////////////////////////////////////////////////////
    // Jumps and conditional jumps
    //
    ///////////////////////////////////////////////////////

    #[inline]
    fn jump(&mut self, to: AddressType) -> Result<()> {
        self.active_mut_frame().set_ip(to);
        Ok(())
    }

    #[inline]
    fn jump_if_false(&mut self, to: AddressType) -> Result<()> {
        if self.peek(0).is_false() {
            self.active_mut_frame().set_ip(to)
        }
        Ok(())
    }

    ///////////////////////////////////////////////////////
    // Return from procedures / closures
    //
    ///////////////////////////////////////////////////////

    fn _return(&mut self) -> Result<Option<Value>> {
        // save the return value
        let value = self.pop();
        let (remaining, frame) = self.pop_frame();

        // unwind the stack
        self.stack.truncate(frame.stack_base);
        self.push(value.clone())?;

        if remaining <= 0 {
            #[cfg(feature = "debug_vm")]
            println!(
                "{}",
                debug::stack::pretty_print(&self.stack, self.active_frame().stack_base)
            );
            Ok(Some(value))
        } else {
            Ok(None)
        }
    }

    ///////////////////////////////////////////////////////
    // Closure creation
    //
    ///////////////////////////////////////////////////////

    fn create_closure(&mut self, addr: ConstAddressType) -> Result<()> {
        match self.read_constant(addr).clone() {
            Value::Procedure(proc) => {
                let up_values = self.open_up_values.values().cloned().collect();
                let closure = Closure::from_rc(proc.as_native().clone(), up_values);
                self.push(Value::Closure(closure))
            }
            _ => return self.compiler_bug("Expected closure function"),
        }
    }

    fn create_up_value(&mut self, addr: AddressType, is_local: bool) -> Result<()> {
        if is_local {
            // capture local as new up-value
            self.capture_up_value(addr)?;
        } else {
            // up-value already exists in outer scope
            let stack_idx = self.frame_slot_address_to_stack_index(addr);
            self.open_up_values
                .insert(stack_idx, self.active_frame().closure.get_up_value(addr));
        }
        Ok(())
    }

    fn capture_up_value(&mut self, addr: AddressType) -> Result<()> {
        let stack_idx = self.frame_slot_address_to_stack_index(addr);

        if self.open_up_values.contains_key(&stack_idx) {
            return Ok(());
        } else {
            let value = self.stack.at(stack_idx as usize).clone();
            self.open_up_values.insert(stack_idx, RefValue::new(value));
            Ok(())
        }
    }

    #[inline]
    fn close_up_value(&mut self, addr: AddressType) -> Result<()> {
        let stack_idx = self.frame_slot_address_to_stack_index(addr);
        self.open_up_values.remove(&stack_idx);
        Ok(())
    }

    ///////////////////////////////////////////////////////
    // Apply procedures and closures
    //
    ///////////////////////////////////////////////////////

    #[inline]
    fn apply(&mut self, args: usize) -> Result<()> {
        let callable = self.peek(args).clone();

        match callable {
            value::Value::Closure(cl) => self.apply_closure(cl.clone(), args)?,
            value::Value::Procedure(procedure::Procedure::Native(p)) => {
                self.apply_native(p.clone(), args)?
            }
            value::Value::Procedure(procedure::Procedure::Foreign(p)) => {
                self.apply_foreign(p.clone(), args)?
            }
            other => {
                return self.runtime_error(error::non_callable(other));
            }
        };
        Ok(())
    }

    #[inline]
    fn apply_closure(&mut self, closure: Closure, arg_count: usize) -> Result<()> {
        self.check_arity(&closure.procedure().arity, arg_count)?;
        let arg_count = self.bind_arguments(&closure.procedure().arity, arg_count)?;
        self.push_frame(closure, arg_count)?;
        #[cfg(feature = "debug_vm")]
        self.disassemble_frame();
        Ok(())
    }

    #[inline]
    fn apply_native(
        &mut self,
        proc: Rc<procedure::native::Procedure>,
        arg_count: usize,
    ) -> Result<()> {
        self.check_arity(&proc.arity, arg_count)?;
        let arg_count = self.bind_arguments(&proc.arity, arg_count)?;
        let closure = proc.into();
        self.push_frame(closure, arg_count)?;
        #[cfg(feature = "debug_vm")]
        self.disassemble_frame();
        Ok(())
    }

    #[inline]
    fn apply_foreign(
        &mut self,
        proc: Rc<procedure::foreign::Procedure>,
        arg_count: usize,
    ) -> Result<()> {
        self.check_arity(&proc.arity, arg_count)?;
        let arguments = self.pop_n(arg_count).iter().cloned().collect();
        match proc.call(arguments) {
            Ok(v) => {
                self.push(v)?;
                Ok(())
            }
            Err(e) => self.runtime_error(e),
        }
    }

    fn bind_arguments(&mut self, arity: &Arity, arg_count: usize) -> Result<usize> {
        match arity {
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

    fn check_arity(&self, arity: &Arity, arg_count: usize) -> Result<()> {
        match arity {
            Arity::Exactly(n) if arg_count == *n => Ok(()),
            Arity::AtLeast(n) if arg_count >= *n => Ok(()),
            Arity::Many => Ok(()),
            other => self.runtime_error(error::arity_mismatch(other.clone(), arg_count)),
        }
    }

    ///////////////////////////////////////////////////////
    // Managing variables in different scopes
    //
    ///////////////////////////////////////////////////////

    fn define_value(&mut self, addr: ConstAddressType) -> Result<()> {
        let v = self.pop();
        let id = self.read_identifier(addr)?;
        self.toplevel.set(id.clone(), v.clone());
        self.push(self.values.unspecified())?;
        Ok(())
    }

    ///////////////////////////////////////////////////////
    // Global variables
    ///////////////////////////////////////////////////////

    #[inline]
    fn get_global(&mut self, addr: ConstAddressType) -> Result<()> {
        let id = self.read_identifier(addr)?;

        if let Some(value) = self.toplevel.get_owned(&id) {
            self.push(value)?;
        } else {
            self.runtime_error(error::undefined_variable(id))?;
        }
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

    ///////////////////////////////////////////////////////
    // Closure variables aka up-values
    ///////////////////////////////////////////////////////

    #[inline]
    fn get_up_value(&mut self, addr: AddressType) -> Result<()> {
        let value = self.active_frame().closure.get_up_value(addr);
        self.push(value.to_value())?;
        Ok(())
    }

    #[inline]
    fn set_up_value(&mut self, addr: AddressType) -> Result<()> {
        let value = self.peek(0).clone();
        self.active_mut_frame().closure.set_up_value(addr, value);
        self.push(self.values.unspecified())?;
        Ok(())
    }

    ///////////////////////////////////////////////////////
    // Local variables
    ///////////////////////////////////////////////////////

    #[inline]
    fn get_local(&mut self, addr: AddressType) -> Result<()> {
        self.push(self.frame_get_slot(addr).clone())
    }

    #[inline]
    fn set_local(&mut self, addr: AddressType) -> Result<()> {
        self.frame_set_slot(addr, self.peek(0).clone());
        self.push(self.values.unspecified())?;
        Ok(())
    }

    ///////////////////////////////////////////////////////
    // Various utilities and helpers
    ///////////////////////////////////////////////////////

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

        println!(
            "{}",
            debug::stack::pretty_print(&self.stack, self.active_frame().stack_base)
        );
        disassembler.disassemble_instruction(chunk, self.active_frame().ip);
    }

    fn disassemble_frame(&mut self) {
        let mut disassembler = Disassembler::new(std::io::stdout());
        let chunk = self.active_frame().code();
        println!("\n");
        disassembler.disassemble(
            chunk,
            &format!(
                "ACTIVE FRAME ({:?})",
                disassembler
                    .disassemble_value(&Value::Closure(self.active_frame().closure.clone()))
            ),
        );
        println!("\n");
    }
}
