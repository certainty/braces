////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// # Stack based virtual machine for our scheme implementation
//
// This is the implementation of the virtual machine for our scheme implementation. It's a standard stack based
// VM that borrows concepts from LUA's VM implementation to make it non-naive. However there is probably still alot
// of room for improvement in terms of performance.
//
// ## Usage
//
// Most of the time you shouldn't have to use `Instance` directly but use the `VM` interface instead.
// That will give you access to high-level functions that compile and run code on the VM. However in case you
// build your own code this low level interface might come in handy.
//
// Examples:
// ```
// // first compile some source
// let source_code = String::from("(let ((x #t)) (lambda () x))");
// let mut source = StringSource::new(source_code, "test");
// let mut compiler  = Compiler::new();
// let unit = compiler.compile_program(&mut source)?;
//
// // instantiate the vm with the top-level bindings and the known values and get the result
//
// let result = Instance::interprete(unit.closure, 256, TopLevel::new(), unit.values)?;
// println!("{:#?}", result);
// ```
//
// ## Internals
//
// What follows is a more in depth documentation of how the VM works and some of the design choices that have been made.
//
//
// ### Design choices
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
pub mod call_frame;
use crate::vm::byte_code::chunk::AddressType;
use rustc_hash::FxHashMap;

use super::byte_code::Instruction;
use super::debug;
use super::disassembler::Disassembler;
use super::global::*;
use super::stack::Stack;
use super::stack_trace::StackTrace;
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
pub type CallStack = Stack<CallFrame>;

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
    // enable cycle debugging
    debug_mode: bool,
}

// TODO: Optimize for performance
// Likely candidates for optimizations are the stack(s)
impl<'a> Instance<'a> {
    pub fn new(
        initial_closure: value::closure::Closure,
        call_stack_size: usize,
        toplevel: &'a mut TopLevel,
        values: &'a mut value::Factory,
        debug_mode: bool,
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
            debug_mode,
        }
    }

    pub fn interprete(
        initial_closure: value::closure::Closure,
        stack_size: usize,
        toplevel: &'a mut TopLevel,
        values: &'a mut value::Factory,
        debug_mode: bool,
    ) -> Result<Value> {
        let mut instance = Self::new(initial_closure, stack_size, toplevel, values, debug_mode);
        instance.run()
    }

    fn run(&mut self) -> Result<Value> {
        self.disassemble_frame();

        loop {
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
    //     ┌───────────────┐
    // 3   │     0x1       │
    //     ├───────────────┤
    // 2   │     0x5       │
    //     ├───────────────┤
    // 1   │     0x10      │
    //     └───────────────┘
    //
    // peek(0) returns 0x1
    // peek(2) returns 0x10

    #[inline]
    fn peek(&self, distance: usize) -> &Value {
        self.stack.peek(distance)
    }

    ///////////////////////////////////////////////////////////
    //
    // Manage the call stack and access frame local variables.
    //
    ///////////////////////////////////////////////////////////

    // Activate a new stack frame with the provided closure and arguments.
    //
    // This prepares the call-stack with the next closures to execute.
    // Note that we always deal with closures to keep the VM code simpler.
    // It doesn't always mean that there are up-values that have been captured.
    //
    // In order to activate the closure the following things have to be done:
    //
    // 1. A new call frame has to be created
    // 2. The base address for the stack frame has to be computed
    // 3. The new call frame is pushed to the call stack
    // 4. The active_frame is set to the new top of the stack
    //
    // ## Stack layout of active closures
    //
    // The closure that is about to be executed will get access to a portion of the value
    // stack which it can use to manage local variables and temporary values. The VM
    // will push the closure itself to the value stack, followed by the values of the arguments
    // for the closure. The position where the closure has been pushed will become the stack_base,
    // which is used  to calculate slot-access for the current active frame.
    //
    // So after `push_frame` has returned the stack will look like this:
    //
    //      ┌─────────────┐
    //  11  │ Arg 2       │
    //      ├─────────────┤
    //  10  │ Arg 1       │
    //      ├─────────────┤
    //  9   │ Arg 0       │
    //      ├─────────────┤
    //  8   │ Closure     │◄───────── stack_base
    //      ├─────────────┤
    //  7   │ Temp        │
    //      ├─────────────┤
    //  6   │ Temp        │
    //      ├─────────────┤
    //  5   │ Temp        │
    //      └─────────────┘
    //
    // The stack base for the closure will be 8 in this case.
    // The data before that belongs to the previously active closure and wont be touched.
    // The values at the stack addresses 9, 10 and 11 will hold the arguments that re provided
    // to the procedure represented by the closure.
    //
    #[inline]
    fn push_frame(&mut self, closure: value::closure::Closure, arg_count: usize) -> Result<()> {
        let base = std::cmp::max(self.stack.len() - arg_count - 1, 0);
        let frame = CallFrame::new(closure, base);
        self.call_stack.push(frame);
        self.active_frame = self.call_stack.top_mut_ptr();
        Ok(())
    }

    // Remove the currently active stack frame
    // and restore the `active_frame` to the previous one.
    //
    // It returns the frame that has been removed and the length
    // of the call-stack after the frame has been popped.
    #[inline]
    fn pop_frame(&mut self) -> (usize, CallFrame) {
        let frame = self.call_stack.pop();
        let len = self.call_stack.len();
        if len > 0 {
            self.active_frame = self.call_stack.top_mut_ptr();
        }
        (len, frame)
    }

    // Retrieve a reference to the currently active frame.
    // This is the always the frame at the top of the call-stack
    // which is currently executed.
    #[inline]
    fn active_frame(&self) -> &CallFrame {
        unsafe { &(*self.active_frame) }
    }

    // Retrieve a mutable reference to the currently active frame.
    //
    // This function is unsafe but is required to efficiently increment
    // the instruction pointer.
    #[inline]
    fn active_mut_frame(&self) -> &mut CallFrame {
        unsafe { &mut (*self.active_frame) }
    }

    // Compute the absolute stack index, from the provided slot_address.
    // A frame slot is the relative address of a value on the value-stack for
    // the currently active frame.
    //
    //     ┌─────────────┐
    //  6  │ Slot3       │
    //     ├─────────────┤
    //  5  │ Slot2       │
    //     ├─────────────┤
    //  4  │ Slot1       │
    //     ├─────────────┤
    //  3  │ Closure     │◄───────── stack_base
    //     ├─────────────┤
    //  2  │ Temp        │
    //     ├─────────────┤
    //  1  │ Temp        │
    //     ├─────────────┤
    //  0  │ Temp        │
    //     └─────────────┘
    //
    // frame_get_slot(0) would return 3.
    // frame_get_slot(1) would return 4.
    //
    // As you can see all access is relative to the stack_base,
    // which is associated with the active frame.
    //
    #[inline]
    fn frame_slot_address_to_stack_index(&self, slot_address: AddressType) -> usize {
        self.active_frame().stack_base + (slot_address as usize) + 1
    }

    // Retrieve the value from a given slot of the currently active frame.
    #[inline]
    fn frame_get_slot(&self, slot_address: AddressType) -> &Value {
        let index = self.frame_slot_address_to_stack_index(slot_address);
        self.stack.at(index)
    }

    #[inline]
    fn frame_set_slot(&mut self, slot_address: AddressType, value: Value) {
        let index = self.frame_slot_address_to_stack_index(slot_address);
        self.stack.set(index, value);
    }

    ///////////////////////////////////////////////////////
    //
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
    // The main job of return is to remove the active stack frame
    // and unwind the stack so that execution can continue.
    // It also makes sure that result of the closure will become the new top value
    // on the stack.
    //
    // ## Stack effect
    //
    // After this function has returned all the locals and temporary values of the active stack frame will have been popped.
    // and the closure will be replaced by the final result of its execution.
    //
    // Stack before return:
    //
    //     ┌─────────────┐
    //  6  │ 'foobar     │ ◄───────── stack top
    //     ├─────────────┤
    //  5  │ #t          │
    //     ├─────────────┤
    //  4  │ #f          │
    //     ├─────────────┤
    //  3  │ Closure     │◄───────── stack_base
    //     ├─────────────┤
    //  2  │ "some"      │
    //     ├─────────────┤
    //  1  │ 'baz        │
    //     ├─────────────┤
    //  0  │ #t          │
    //     └─────────────┘
    //
    // Stack after return:
    //
    //     ┌─────────────┐
    //  3  │ 'foobar     │ ◄───────── stack top
    //     ├─────────────┤
    //  2  │ "some"      │
    //     ├─────────────┤
    //  1  │ 'baz        │
    //     ├─────────────┤
    //  0  │ #t          │
    //     └─────────────┘
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
            self.debug_stack();
            Ok(Some(value))
        } else {
            Ok(None)
        }
    }

    ///////////////////////////////////////////////////////
    // Closure creation
    //
    // Reads the procedure from the address specified by `addr`
    // and creates a closure out of it. A closure is code + up-values so this
    // collects all the `open_up_values` and provides these to the closure.
    //
    // ## Stack effect
    //
    // The top of the stack will hold the resulting closure.
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

    ////////////////////////////////////////////////////////////////////////////
    //
    // Creates an up-value from the value add the slot-address provided by `addr`.
    //
    // This adds the up-value to the currently open ones. They will be closed as soon
    // as the local variables they capture get out of scope.
    //
    // If the value is a local it is captured.
    // Otherwise it is already an up-value in the active closure and we can add it from there.
    //
    // ## Stack effect
    // None
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

    // Up values are indexed by absolute stack address.
    // This function captures the variable at `addr`
    // and adds it to the currently open up-values.
    //
    // ## Stack effect
    // None
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

    // Close currently open up-values
    //
    // This removes the up-value resulted from `addr` from the open-up-values.
    // This happens when the variable that is associated with `addr` goes
    // out of scope.
    #[inline]
    fn close_up_value(&mut self, addr: AddressType) -> Result<()> {
        let stack_idx = self.frame_slot_address_to_stack_index(addr);
        self.open_up_values.remove(&stack_idx);
        Ok(())
    }

    ///////////////////////////////////////////////////////
    //
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

    ///////////////////////////////////////////////////////
    // Apply closures to the supplied arguments
    //
    //
    // ## Stack effect
    //
    // Pushes the arguments onto the stack
    //
    #[inline]
    fn apply_closure(&mut self, closure: Closure, arg_count: usize) -> Result<()> {
        self.check_arity(&closure.procedure().arity, arg_count)?;
        let arg_count = self.bind_arguments(&closure.procedure().arity, arg_count)?;
        self.push_frame(closure, arg_count)?;
        self.disassemble_frame();
        Ok(())
    }

    ///////////////////////////////////////////////////////
    // Apply a native procedure to the supplied arguments
    //
    //
    // ## Stack effect
    //
    //
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
        self.disassemble_frame();
        Ok(())
    }
    ///////////////////////////////////////////////////////
    // Apply a native procedure to the supplied arguments
    //
    //
    // ## Call stack
    //
    // Foreign procedure calls don't result in a call-frame being pushed.
    // Instead the VM directly executes the foreign procedures and pushes the result
    // onto the stack.
    //
    // ## Stack effect
    //
    // Pushes the result of the application onto the stack.
    //
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

    ////////////////////////////////////////////////////////////////////////
    //
    // Provide the arguments for the procedure that is about to be executed.
    //
    // Arguments are just local values, which means they are represented
    // as values on the stack. The called function accesses them in the usual
    // manner via direct stack access.
    //
    // This function takes care of transferring the arguments correctly according
    // to the function's arity. This means there are three cases to consider:
    //
    // ## 1. Exact amount of arguments
    //
    // In that case there is not much to do, since the variables will already be in place on the stack.
    //
    // ## 2. At least n arguments
    //
    // In this case the function makes sure that at least n arguments are supplied. This fist n are
    // already at the right place on the stack. However all the additional arguments, if supplied, will
    // be represented with a single variable, which holds a list. This means extra arguments are popped
    // from the stack and new list value holding the values of those variables will be pushed onto the stack.
    //
    // argc = 4 and n = 2
    //
    //      ┌──────────────┐
    //   3  │ 'foo         │
    //      ├──────────────┤               ┌──────────────┐
    //   2  │ 'bar         │            2  │ '(bar foo)   │
    //      ├──────────────┤  ────►        ├──────────────┤
    //   1  │ 'baz         │            1  │ 'baz         │
    //      ├──────────────┤               ├──────────────┤
    //   0  │ 'fro         │            0  │ 'fro         │
    //      └──────────────┘               └──────────────┘
    //
    // ## 3. Variable amount of arguments
    //
    // In this case all provided arguments will be accumulated into a single local variable (single slot value)
    // which will hold the list of provided values.
    //
    // args = 4
    //
    //       ┌──────────────┐
    //    3  │ 'foo         │
    //       ├──────────────┤
    //    2  │ 'bar         │            ┌──────────────────┐
    //       ├──────────────┤  ────►  0  │'(fro baz bar foo)│
    //    1  │ 'baz         │            └──────────────────┘
    //       ├──────────────┤
    //    0  │ 'fro         │
    //       └──────────────┘

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
    //
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
            StackTrace::from(&self.call_stack),
        ));
        result
    }

    // Debug the VM
    fn debug_cycle(&mut self) {
        if !self.debug_mode {
            return;
        }

        let mut disassembler = Disassembler::new(std::io::stdout());
        let chunk = self.active_frame().code();

        println!(
            "{}",
            debug::stack::pretty_print(&self.stack, self.active_frame().stack_base)
        );
        disassembler.disassemble_instruction(chunk, self.active_frame().ip);
    }

    fn debug_stack(&mut self) {
        if !self.debug_mode {
            return;
        }

        println!(
            "{}",
            debug::stack::pretty_print(&self.stack, self.active_frame().stack_base)
        );
    }

    fn disassemble_frame(&mut self) {
        if !self.debug_mode {
            return;
        }

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
