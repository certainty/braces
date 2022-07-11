////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///
/// # Stack based virtual machine for our scheme implementation
///
/// This is the implementation of the virtual machine for our scheme implementation. It's a standard stack based
/// VM that borrows concepts from LUA's VM implementation to make it non-naive. However, there is probably still a lot
/// of room for improvement in terms of performance.
///
/// ## Usage
///
/// Most of the time you shouldn't have to use `Instance` directly but use the `VM` interface instead.
/// That will give you access to high-level functions that compile and run code on the VM. However, in case you
/// build your own code this low level interface might come in handy.
///
/// Examples:
/// ```
/// use braces::vm::instance::{Instance, Options};
/// use braces::vm::{value, global::TopLevel, VM};
/// use braces::compiler::{source::StringSource, Compiler};
/// let mut source = StringSource::new("(define (id x) x) (id #t)");
/// let mut compiler  = Compiler::new();
/// let unit = compiler.compile(&mut source).unwrap();
/// // Now interpret the unit
/// let mut top_level = TopLevel::new();
/// let mut values = value::Factory::default();
/// let result = Instance::interpret(unit.closure, &mut top_level, &mut values, Options::default()).unwrap();
/// println!("{:#?}", result);
/// ```
///
///
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
pub mod call_frame;
use crate::compiler::source::FileSource;
use crate::compiler::Compiler;
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
use crate::vm::value::access::{Access, Reference};
use call_frame::CallFrame;
use std::rc::Rc;

type Result<T> = std::result::Result<T, Error>;

type ValueStack = Stack<Access<Value>>;
pub type CallStack = Stack<CallFrame>;

pub struct Options {
    pub stack_size: usize,
    pub debug_mode: bool,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            stack_size: 64,
            debug_mode: false,
        }
    }
}

pub struct Instance<'a> {
    // The value factory which can be shared between individual instance runs.
    // The sharing is needed only in the `Repl` where we want to define bindings as we go
    // and remember them for the next run of the `VM`.
    pub(crate) values: &'a mut value::Factory,
    // top level environment which can be shared between individual instance runs
    top_level: &'a mut TopLevel,
    // a simple stack to manage intermediate values and locals
    stack: ValueStack,
    // manage all live functions
    call_stack: CallStack,
    // the currently active stack frame
    active_frame: *mut CallFrame,
    // open up-values are indexed by absolute stack address
    open_up_values: FxHashMap<AddressType, Reference<Value>>,
    // enable cycle debugging
    settings: Options,
}

// TODO: Optimize for performance
// Likely candidates for optimizations are the stack(s)
impl<'a> Instance<'a> {
    pub fn new(
        initial_closure: value::closure::Closure,
        top_level: &'a mut TopLevel,
        values: &'a mut value::Factory,
        options: Options,
    ) -> Self {
        let mut vm = Self::vanilla(top_level, values, options);
        vm.push(Value::Closure(initial_closure.clone())).unwrap();
        vm.push_frame(initial_closure, 0).unwrap();
        vm
    }

    pub fn vanilla(
        top_level: &'a mut TopLevel,
        values: &'a mut value::Factory,
        settings: Options,
    ) -> Self {
        let stack = ValueStack::new(settings.stack_size * 255);
        let call_stack = CallStack::new(settings.stack_size);
        let open_up_values = FxHashMap::<AddressType, Reference<Value>>::default();

        Self {
            values,
            stack,
            call_stack,
            top_level,
            active_frame: std::ptr::null_mut(),
            open_up_values,
            settings,
        }
    }

    pub fn interpret(
        initial_closure: value::closure::Closure,
        top_level: &'a mut TopLevel,
        values: &'a mut value::Factory,
        options: Options,
    ) -> Result<Value> {
        let mut instance = Self::new(initial_closure, top_level, values, options);
        instance.run()
    }

    pub fn interpret_expander(
        expander: procedure::Procedure,
        syntax: &Value,
        arguments: &[Value],
        top_level: &'a mut TopLevel,
        values: &'a mut value::Factory,
    ) -> Result<Value> {
        let mut vm = Self::vanilla(top_level, values, Options::default());
        let is_native = expander.is_native();

        vm.push(Value::Procedure(expander))?;
        vm.push(syntax.clone())?;
        for arg in arguments {
            vm.push(arg.clone())?
        }
        vm.apply_tail_call(arguments.len() + 1)?;

        if is_native {
            vm.run()
        } else {
            Ok(vm.stack.pop().into_inner())
        }
    }

    pub fn gensym(&mut self) -> Value {
        self.values.gensym(None)
    }

    pub fn load_file(&mut self, path: &std::path::Path) -> Result<Access<Value>> {
        let mut source = FileSource::new(path.to_owned());
        let mut compiler = Compiler::new();
        let loaded_file_closure = compiler.compile(&mut source)?.closure;

        self.push(Value::Closure(loaded_file_closure.clone()))?;
        self.push_frame(loaded_file_closure, 0)?;
        self.apply_tail_call(0)?;
        Ok(self.stack.pop())
    }

    fn run(&mut self) -> Result<Value> {
        self.disassemble_frame();

        loop {
            self.debug_cycle();

            match self.next_instruction() {
                &Instruction::True => self.push(self.values.bool_true())?,
                &Instruction::False => self.push(self.values.bool_false())?,
                &Instruction::Nil => self.push(self.values.nil())?,
                &Instruction::Const(address) => self.push(self.read_constant(address).clone())?,

                &Instruction::Define(address) => self.define(address)?,

                &Instruction::GetGlobal(address) => self.fetch_global(address)?,
                &Instruction::UpValue(address, is_local) => {
                    self.create_up_value(address, is_local)?
                }
                &Instruction::CloseUpValue(address) => self.close_up_value(address)?,
                &Instruction::GetUpValue(address) => self.fetch_up_value(address)?,
                &Instruction::GetLocal(address) => self.fetch_local(address)?,
                &Instruction::Set => self.set()?,
                &Instruction::Closure(address) => self.create_closure(address)?,
                &Instruction::Apply(args) => self.apply(args)?,
                &Instruction::ApplyTCO(args) => self.apply_tail_call(args)?,
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
    fn read_constant(&self, address: ConstAddressType) -> &Value {
        self.active_frame().code().read_constant(address)
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
    fn push<T: Into<Access<Value>>>(&mut self, v: T) -> Result<()> {
        self.stack.push(v.into());
        Ok(())
    }

    #[inline]
    fn pop(&mut self) -> Access<Value> {
        self.stack.pop().into()
    }

    #[inline]
    fn pop_n(&mut self, n: usize) -> Vec<Access<Value>> {
        self.stack.pop_n(n)
    }

    #[inline]
    fn stack_slice_mut(&mut self, n: usize) -> &mut [Access<Value>] {
        self.stack.top_n_mut(n)
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
    fn peek(&self, distance: usize) -> &Access<Value> {
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
    // The data before that belongs to the previously active closure and won't be touched.
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

    ///////////////////////////////////////////////////////////////////////////////////
    // Reuse the current stack frame to set up a tail call
    //
    // For the documentation of the stack layout before a call check the documentation of `push_frame`.
    //
    // This function re-uses the currently active frame in a tail-call.
    // Tail calls are special in that the result is not required to continue the computation.
    // This also means we don't need to push a new stack-frame but can re-use the current stack frame instead
    //
    #[inline]
    fn overwrite_frame(
        &mut self,
        closure: value::closure::Closure,
        arg_count: usize,
    ) -> Result<()> {
        if self.has_active_frame() {
            //re-use the current frame for tail calls
            let base = std::cmp::max(self.stack.len() - arg_count - 1, 0);
            self.active_mut_frame().stack_base = base;
            self.active_mut_frame().closure = closure;
            self.active_mut_frame().set_ip(0);
        } else {
            self.push_frame(closure, arg_count)?;
        }
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
    // This is always the frame at the top of the call-stack
    // which is currently executed.
    #[inline]
    fn active_frame(&self) -> &CallFrame {
        unsafe { &(*self.active_frame) }
    }

    #[inline]
    fn has_active_frame(&self) -> bool {
        !self.active_frame.is_null()
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
    fn frame_get_slot(&self, slot_address: AddressType) -> &Access<Value> {
        let index = self.frame_slot_address_to_stack_index(slot_address);
        self.stack.at(index)
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
        let is_false = match self.peek(0) {
            Access::ByRef(r) => r.get_inner_ref().is_false(),
            Access::ByVal(v) => v.is_false(),
        };

        if is_false {
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
        let value = self.pop().to_owned(); // unwrap any references
        let (remaining, frame) = self.pop_frame();

        // unwind the stack
        self.stack.truncate(frame.stack_base);

        if remaining <= 0 {
            self.push(value.clone())?;
            self.debug_stack();
            Ok(Some(value))
        } else {
            self.push(value)?;
            Ok(None)
        }
    }

    ///////////////////////////////////////////////////////
    // Closure creation
    //
    // Reads the procedure from the address specified by `address`
    // and creates a closure out of it. A closure is code + up-values so this
    // collects all the `open_up_values` and provides these to the closure.
    //
    // ## Stack effect
    //
    // The top of the stack will hold the resulting closure.
    fn create_closure(&mut self, address: ConstAddressType) -> Result<()> {
        match self.read_constant(address).clone() {
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
    // Creates an up-value from the value add the slot-address provided by `address`.
    //
    // This adds the up-value to the currently open ones. They will be closed as soon
    // as the local variables they capture get out of scope.
    //
    // If the value is a local it is captured.
    // If not, it is already an up-value in the active closure, and we can add it from there.
    //
    // ## Stack effect
    // None
    fn create_up_value(&mut self, address: AddressType, is_local: bool) -> Result<()> {
        if is_local {
            // capture local as new up-value
            self.capture_up_value(address)?;
        } else {
            // up-value already exists in outer scope
            let stack_idx = self.frame_slot_address_to_stack_index(address);
            self.open_up_values
                .insert(stack_idx, self.active_frame().closure.get_up_value(address));
        }
        Ok(())
    }

    // Up values are indexed by absolute stack address.
    // This function captures the variable at `address`
    // and adds it to the currently open up-values.
    //
    // ## Stack effect
    // None
    fn capture_up_value(&mut self, address: AddressType) -> Result<()> {
        let stack_idx = self.frame_slot_address_to_stack_index(address);

        if self.open_up_values.contains_key(&stack_idx) {
            return Ok(());
        } else {
            let value = self.stack.at(stack_idx as usize).to_owned();
            self.open_up_values
                .insert(stack_idx, Reference::from(value));
            Ok(())
        }
    }

    // Close currently open up-values
    //
    // This removes the up-value resulted from `address` from the open-up-values.
    // This happens when the variable that is associated with `address` goes
    // out of scope.
    #[inline]
    fn close_up_value(&mut self, address: AddressType) -> Result<()> {
        let stack_idx = self.frame_slot_address_to_stack_index(address);
        self.open_up_values.remove(&stack_idx);
        Ok(())
    }

    ///////////////////////////////////////////////////////
    //
    // Apply procedures and closures
    //
    ///////////////////////////////////////////////////////

    fn apply(&mut self, args: usize) -> Result<()> {
        let callable = self.peek(args).clone();
        let result = match callable {
            Access::ByRef(r) => self._apply(&r.get_inner_ref(), args),
            Access::ByVal(v) => self._apply(&v, args),
        };
        result
    }

    fn _apply(&mut self, callable: &Value, args: usize) -> Result<()> {
        match callable {
            value::Value::Closure(cl) => self.apply_closure(cl.clone(), args)?,
            value::Value::Procedure(procedure::Procedure::Native(p)) => {
                self.apply_native(p.clone(), args)?
            }
            value::Value::Procedure(procedure::Procedure::Foreign(p)) => {
                self.apply_foreign(p.clone(), args)?
            }
            other => {
                return self.runtime_error(error::non_callable(other.clone()), None);
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
    // Instead, the VM directly executes the foreign procedures and pushes the result
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
        let arguments = self
            .pop_n(arg_count)
            .iter()
            .map(|a| a.clone().to_owned())
            .collect();
        // also pop the procedure itself
        self.pop();
        match proc.call(self, arguments) {
            Ok(v) => {
                self.push(v)?;
                Ok(())
            }
            Err(e) => {
                println!("Error in foreign function: {} {:?}", proc.name.clone(), e);
                self.runtime_error(e, Some(proc.name.clone()))
            }
        }
    }

    // rewind the arguments so that they can be overwritten with new arguments of the tail call
    #[inline]
    fn setup_tail_call(&mut self, args: usize) -> Result<()> {
        // prepare the stack
        // all the arguments are now at the top of the stack
        // we transfer them to the start of the frame_base here, which is safe to do only at this point
        // since all local variable references have been resolved, and we're right before the call
        let arguments = self.stack.pop_n(args);
        // now save the last value
        let value = self.pop();

        // prepare stack frame for overwrite
        if self.has_active_frame() {
            self.stack.truncate(self.active_frame().stack_base);
        }
        // restore the saved value
        self.push(value)?;
        // push the arguments again
        for arg in arguments {
            self.push(arg)?
        }
        Ok(())
    }

    fn apply_tail_call(&mut self, args: usize) -> Result<()> {
        self.setup_tail_call(args)?;
        let callable = self.peek(args).clone();

        match callable {
            Access::ByRef(r) => self._apply_tail_call(&r.get_inner_ref(), args),
            Access::ByVal(v) => self._apply_tail_call(&v, args),
        }
    }

    fn _apply_tail_call(&mut self, callable: &Value, args: usize) -> Result<()> {
        match callable {
            value::Value::Closure(cl) => self.tail_call_closure(cl.clone(), args)?,
            value::Value::Procedure(procedure::Procedure::Native(p)) => {
                self.tail_call_native(p.clone(), args)?
            }
            value::Value::Procedure(procedure::Procedure::Foreign(p)) => {
                // always tail call in the sense that it doesn't create a call-frame anyways
                self.apply_foreign(p.clone(), args)?
            }
            other => {
                return self.runtime_error(error::non_callable(other.clone()), None);
            }
        };
        Ok(())
    }

    ///////////////////////////////////////////////////////
    // Tail call closure with supplied arguments
    // A tail call re-uses the current stack frame instead of pushing a new one.
    //
    // ## Stack effect
    //
    // Pushes the result of the procedure onto the stack
    //
    #[inline]
    fn tail_call_closure(&mut self, closure: Closure, arg_count: usize) -> Result<()> {
        self.check_arity(&closure.procedure().arity, arg_count)?;
        // make sure the previous arguments are reset on the stack, so we can provide the new ones
        // since the function doesn't really return we can simply discard the arguments
        let arg_count = self.bind_arguments(&closure.procedure().arity, arg_count)?;

        // prepare the top frame for the tail call
        self.overwrite_frame(closure, arg_count)?;
        self.disassemble_frame();
        Ok(())
    }

    ///////////////////////////////////////////////////////
    // Tail call a native procedure with supplied arguments.
    // A tail call re-uses the current stack frame instead of pushing a new one.
    //
    // This allows (tail) recursive procedures to be implemented with constant
    // stack space. So, without risking a stack overflow.
    //
    // ## Stack effect
    //
    // Replaces the top of the stack with the result of the procedure
    //////////////////////////////////////////////////////
    #[inline]
    fn tail_call_native(
        &mut self,
        proc: Rc<procedure::native::Procedure>,
        arg_count: usize,
    ) -> Result<()> {
        self.check_arity(&proc.arity, arg_count)?;
        let arg_count = self.bind_arguments(&proc.arity, arg_count)?;
        let closure = proc.into();

        // prepare the top frame for the tail call
        self.overwrite_frame(closure, arg_count)?;
        self.disassemble_frame();
        Ok(())
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
    // already at the right place on the stack. However, all the additional arguments, if supplied, will
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

    // Make sure to check that the arity matches th arg_count before you call this function
    fn bind_arguments(&mut self, arity: &Arity, arg_count: usize) -> Result<usize> {
        match arity {
            Arity::Exactly(n) => {
                let stack_slice = self.stack_slice_mut(*n);
                // we place the values on the stack into references, which are then available during the function call
                for i in 0..*n {
                    // re-package the value as a reference thus effectively binding it
                    // to the variable that represents that argument
                    stack_slice[i] = Access::ByRef(Reference::from(stack_slice[i].to_owned()));
                }

                self.debug_stack();
                Ok(arg_count)
            }
            Arity::AtLeast(n) => {
                let stack_slice = self.stack_slice_mut(arg_count);

                // bind the positional arguments first
                for i in 0..*n {
                    stack_slice[i] = Access::ByRef(Reference::from(stack_slice[i].to_owned()));
                }

                // now stuff the rest into a list and bind that to the last argument
                let rest_args = self.pop_n(arg_count - n);
                let rest_list = self.values.proper_list(
                    rest_args
                        .iter()
                        .map(|e| e.clone().to_owned())
                        .collect::<Vec<_>>()
                        .into(),
                );

                self.push(Access::ByRef(Reference::from(rest_list)))?;

                self.debug_stack();
                Ok(n + 1)
            }
            Arity::Many => {
                let rest_values = self
                    .pop_n(arg_count)
                    .iter()
                    .map(|e| e.clone().to_owned())
                    .collect();
                let rest_list = self.values.proper_list(rest_values);
                self.push(Access::ByRef(Reference::from(rest_list)))?;

                self.debug_stack();
                Ok(1)
            }
        }
    }

    fn check_arity(&self, arity: &Arity, arg_count: usize) -> Result<()> {
        match arity {
            Arity::Exactly(n) if arg_count == *n => Ok(()),
            Arity::AtLeast(n) if arg_count >= *n => Ok(()),
            Arity::Many => Ok(()),
            other => self.runtime_error(error::arity_mismatch(other.clone(), arg_count), None),
        }
    }

    ///////////////////////////////////////////////////////
    //
    // Managing variables in different scopes
    //
    ///////////////////////////////////////////////////////

    fn define(&mut self, address: ConstAddressType) -> Result<()> {
        let v = self.pop();
        let id = self.read_identifier(address)?;
        self.top_level.define(id, v.to_owned());
        self.push(self.values.unspecified())?;
        Ok(())
    }

    ///////////////////////////////////////////////////////
    // Global variables
    ///////////////////////////////////////////////////////

    #[inline]
    fn fetch_global(&mut self, address: ConstAddressType) -> Result<()> {
        let id = self.read_identifier(address)?;
        let value = self.top_level.get(&id).cloned();

        if let Some(reference) = value {
            self.push(Access::ByRef(reference))?;
        } else {
            self.runtime_error(error::undefined_variable(id), None)?;
        }
        Ok(())
    }

    ///////////////////////////////////////////////////////
    // Closure variables aka up-values
    ///////////////////////////////////////////////////////

    #[inline]
    fn fetch_up_value(&mut self, address: AddressType) -> Result<()> {
        let value = self.active_frame().closure.get_up_value(address);
        self.push(Access::ByRef(value))?;
        Ok(())
    }

    ///////////////////////////////////////////////////////
    // Local variables
    ///////////////////////////////////////////////////////

    #[inline]
    fn fetch_local(&mut self, address: AddressType) -> Result<()> {
        self.push(self.frame_get_slot(address).clone())
    }

    // the stack before the call to stack looks like this:
    //       ┌──────────────┐
    //    3  │ value        │
    //       ├──────────────┤
    //    2  │ location     │
    //       └──────────────┘
    //
    // set replaces the value in location with the value on the top of the stack
    fn set(&mut self) -> Result<()> {
        let value = self.pop();
        let location = self.pop();

        match location {
            Access::ByRef(mut r) => r.set(value.to_owned()),
            _ => {
                return self.runtime_error(
                    error::argument_error(location.to_owned(), "Can't set! immutable value"),
                    None,
                )
            }
        }

        self.push(self.values.unspecified())
    }

    ///////////////////////////////////////////////////////
    // Various utilities and helpers
    ///////////////////////////////////////////////////////
    fn read_identifier(&mut self, address: ConstAddressType) -> Result<Symbol> {
        if let Value::Symbol(s) = self.read_constant(address) {
            Ok(s.clone())
        } else {
            self.compiler_bug(&format!("Expected symbol at address: {}", address))
        }
    }

    #[inline]
    fn compiler_bug<T>(&mut self, message: &str) -> Result<T> {
        let result = Err(Error::CompilerBug(message.to_string()));
        self.stack_reset()?;
        result
    }

    fn runtime_error<T>(&self, e: error::RuntimeError, context: Option<String>) -> Result<T> {
        if self.has_active_frame() {
            let result = Err(Error::RuntimeError(
                e,
                self.active_frame()
                    .line_number_for_current_instruction()
                    .unwrap_or(0),
                StackTrace::from(&self.call_stack),
                context,
            ));
            result
        } else {
            Err(Error::RuntimeError(e, 0, StackTrace::empty(), context))
        }
    }

    // Debug the VM
    fn debug_cycle(&mut self) {
        if !self.settings.debug_mode {
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
        if !self.settings.debug_mode {
            return;
        }

        println!(
            "{}",
            debug::stack::pretty_print(&self.stack, self.active_frame().stack_base)
        );
    }

    fn disassemble_frame(&mut self) {
        if !self.settings.debug_mode {
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

#[cfg(test)]
mod tests {
    use crate::vm::global::TopLevel;
    use crate::vm::instance::{Instance, Options};
    use crate::vm::value::access::{Access, Reference};
    use crate::vm::value::procedure::Arity;
    use crate::vm::value::{Factory, Value};

    #[test]
    fn test_bind_arguments_exactly_n() -> super::Result<()> {
        let mut top_level = TopLevel::new();
        let mut values = Factory::default();
        let settings = Options::default();
        let mut instance = Instance::vanilla(&mut top_level, &mut values, settings);

        instance.push(Access::ByVal(Value::Bool(true)))?;
        instance.push(Access::ByVal(Value::Bool(false)))?;

        instance.bind_arguments(&Arity::Exactly(2), 2)?;

        assert_eq!(
            instance.stack.as_vec().as_slice(),
            &[
                Access::ByRef(Reference::from(Value::Bool(true))),
                Access::ByRef(Reference::from(Value::Bool(false)))
            ]
        );

        Ok(())
    }

    #[test]
    fn test_bind_arguments_rest_args() -> super::Result<()> {
        let mut top_level = TopLevel::new();
        let mut values = Factory::default();
        let expected_rest_args = values.proper_list(vec![Value::Bool(false), Value::Bool(false)]);

        let mut instance = Instance::vanilla(&mut top_level, &mut values, Options::default());

        instance.push(Access::ByVal(Value::Bool(true)))?;
        instance.push(Access::ByVal(Value::Bool(false)))?;
        instance.push(Access::ByVal(Value::Bool(false)))?;
        instance.push(Access::ByVal(Value::Bool(false)))?;

        instance.bind_arguments(&Arity::AtLeast(2), 4)?;

        assert_eq!(
            instance.stack.as_vec().as_slice(),
            &[
                Access::ByRef(Reference::from(Value::Bool(true))),
                Access::ByRef(Reference::from(Value::Bool(false))),
                Access::ByRef(Reference::from(expected_rest_args))
            ]
        );

        Ok(())
    }
}
