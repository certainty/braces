use super::byte_code::chunk::{Chunk, LineNumber};
use super::byte_code::Instruction;
use super::debug;
#[cfg(feature = "debug_vm")]
use super::disassembler::Disassembler;
use super::global::*;
use super::scheme::value;
use super::scheme::value::lambda::Procedure;
use super::scheme::value::{Symbol, Value};
use super::stack::Stack;
use super::Error;
use crate::vm::byte_code::chunk::ConstAddressType;
use std::rc::Rc;

//////////////////////////////////////////////////
// Welcome the call stack
/////////////////////////////////////////////////

// A callframe is a piece of control data
// that is associated with every live-function

#[derive(Debug)]
pub struct CallFrame {
    pub proc: Rc<Procedure>,
    pub ip: usize,
    pub stack_base: usize,
}

impl CallFrame {
    pub fn new(proc: Rc<Procedure>, stack_base: usize) -> Self {
        Self {
            ip: 0,
            stack_base,
            proc,
        }
    }

    #[inline]
    pub fn code(&self) -> &Chunk {
        self.proc.code()
    }

    #[inline]
    pub fn line_number_for_current_instruction(&self) -> Option<LineNumber> {
        self.proc.code().find_line(self.ip).map(|e| e.2)
    }
}

//////////////////////////////////////////////////////////////////////////////

const FRAMES_MAX: usize = 64;
type StackValue = Rc<Value>;
type ValueStack = Stack<StackValue>;
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
    pub fn new(
        proc: value::lambda::Procedure,
        stack_size: usize,
        toplevel: &'a mut TopLevel,
        values: &'a mut value::Factory,
    ) -> Self {
        let mut stack = ValueStack::new(stack_size);
        let mut call_stack = CallStack::new(FRAMES_MAX);
        let initial_procedure = Rc::new(proc);

        // the first value on the stack is the initial procedure
        stack.push(Rc::new(Value::Procedure(initial_procedure.clone())));

        // the first active stack frame is that of the current procedure
        call_stack.push(CallFrame::new(initial_procedure.clone(), 0));

        let active_frame = call_stack.top_mut_ptr();

        Self {
            values,
            stack,
            call_stack,
            toplevel,
            active_frame,
        }
    }

    pub fn interprete(
        proc: value::lambda::Procedure,
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
                &Instruction::True => self.push_value(self.values.bool_true())?,
                &Instruction::False => self.push_value(self.values.bool_false())?,
                &Instruction::Nil => self.push_value(self.values.nil())?,
                &Instruction::Const(addr) => self.push_value(self.read_constant(addr).clone())?,
                &Instruction::Return => {
                    let value = self.pop();

                    if self.pop_frame() <= 0 {
                        return Ok((*value).clone());
                    } else {
                        self.push(value)?
                    }
                }
                &Instruction::Get(addr) => {
                    let id = self.read_identifier(addr)?;

                    if let Some(value) = self.toplevel.get_owned(&id) {
                        self.push_value(value)?;
                    } else {
                        return self.runtime_error(&format!("Variable {} is unbound", id.as_str()));
                    }
                }
                &Instruction::GetLocal(addr) => {
                    self.push(self.frame_get_slot(addr))?;
                }
                &Instruction::SetLocal(addr) => self.frame_set_slot(addr, self.peek(0)),
                &Instruction::Set(addr) => self.set_value(addr)?,
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
    fn push_value(&mut self, v: Value) -> Result<()> {
        self.stack.push(Rc::new(v));
        Ok(())
    }

    #[inline]
    fn push(&mut self, v: StackValue) -> Result<()> {
        self.stack.push(v);
        Ok(())
    }

    #[inline]
    fn pop(&mut self) -> StackValue {
        self.stack.pop()
    }

    #[inline]
    fn peek(&self, distance: usize) -> StackValue {
        self.stack.peek(distance).clone()
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
        let frame = CallFrame::new(proc, base);
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
    fn frame_get_slot(&self, slot_address: ConstAddressType) -> StackValue {
        self.peek(self.stack_frame_address(slot_address))
    }

    #[inline]
    fn frame_set_slot(&mut self, slot_address: ConstAddressType, value: StackValue) {
        self.stack
            .set(self.stack_frame_address(slot_address), value);
    }

    #[inline]
    fn stack_frame_address(&self, slot_address: ConstAddressType) -> usize {
        self.active_frame().stack_base + (slot_address as usize)
    }

    // specific VM instructions
    #[inline]
    fn apply(&mut self, args: usize) -> Result<()> {
        if let value::Value::Procedure(proc) = &*self.peek(args) {
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
        self.toplevel.set(id.clone(), (*v).clone());
        self.push_value(self.values.unspecified())?;
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
            self.toplevel.set(id.clone(), (*v).clone());
            self.push_value(self.values.unspecified())?;
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

    // Debug the VM

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
        let base_addr = self.active_frame().stack_base;
        let chunk = self.active_frame().code();
        let proc_name = self.active_frame().proc.name();
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
