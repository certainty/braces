use super::call_frame::CallFrame;
use super::VMResult;
use crate::vm::byte_code::{chunk, OpCode};
use crate::vm::disassembler;
use crate::vm::environment::Environment;
use crate::vm::error::VmError;
use crate::vm::printer;
use crate::vm::value::procedure::{Arity, ForeignLambda};
use crate::vm::value::symbol;
use crate::vm::value::Value;

type Result<T> = std::result::Result<T, VmError>;

pub struct Instance<'a> {
    ip: chunk::AddressType,
    current_chunk: &'a chunk::Chunk,
    top_frame: &'a CallFrame<'a>,
    frames: Vec<CallFrame<'a>>,
    stack: Vec<Value>,
}

impl<'a> Instance<'a> {
    pub fn interprete(
        chunk: chunk::Chunk,
        symbols: &mut symbol::SymbolTable,
        env: &mut Environment,
        stack_size: usize,
    ) -> VMResult {
        let frame = CallFrame::root(chunk, env);

        Instance {
            stack: Vec::with_capacity(stack_size),
            frames: vec![],
            current_chunk: &frame.procedure.chunk,
            top_frame: &frame,
            ip: 0,
        }
        .run()
    }

    fn run(&mut self) -> VMResult {
        loop {
            #[cfg(feature = "debug_vm")]
            self.debug_cycle();

            match self.read_op_code() {
                &OpCode::Halt => {
                    return Ok(Some(self.pop()));
                }
                &OpCode::Get => {
                    self.run_get()?;
                }
                &OpCode::Sym(interned) => self
                    .stack
                    .push(Value::Symbol(symbol::Symbol::Interned(interned.clone()))),
                &OpCode::Const(addr) => {
                    let val = self.current_chunk.read_constant(addr);
                    self.stack.push(val.clone());
                }
                &OpCode::Apply => {
                    self.run_apply()?;
                }
                &OpCode::Nop => {
                    //this should never happen
                    panic!("BUG in the compiler detected");
                }
            }
        }
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn pop_n(&mut self, size: usize) -> Vec<Value> {
        let mut result: Vec<Value> = Vec::with_capacity(size);

        for _ in 0..size {
            result.push(self.pop());
        }

        result
    }

    fn run_get(&mut self) -> Result<()> {
        match self.pop() {
            Value::Symbol(symbol::Symbol::Interned(sym)) => match self.top_frame.env.get(&sym) {
                Some(value) => {
                    self.stack.push(value.clone());
                    Ok(())
                }
                _ => Err(VmError::RuntimeError(format!(
                    "Unbound variable `{:?}`",
                    sym
                ))),
            },
            _ => Err(VmError::RuntimeError("Not a symbol".to_string())),
        }
    }

    fn run_apply(&mut self) -> Result<()> {
        match &self.pop() {
            Value::ForeignProcedure(foreign) => self.run_foreign_apply(&foreign),
            _ => Ok(()),
        }
    }

    fn run_foreign_apply(&mut self, foreign: &ForeignLambda) -> Result<()> {
        let args = match foreign.arity {
            Arity::Fixed(count) => self.pop_n(count.into()),
        };

        match (foreign.operation)(&args) {
            Err(e) => Err(VmError::RuntimeError(format!(
                "Error calling foreign lambda: {}",
                e
            ))),
            Ok(value) => {
                self.stack.push(value);
                Ok(())
            }
        }
    }

    fn read_op_code(&mut self) -> &OpCode {
        let code = self.current_chunk.read_opcode(self.ip);
        self.ip = self.ip + 1;
        code
    }

    fn debug_cycle(&self) {
        self.print_stack_trace();
        disassembler::disassemble_instruction(&mut std::io::stdout(), &self.current_chunk, self.ip);
    }

    fn print_stack_trace(&self) {
        print!("     ");
        for value in self.stack.iter() {
            print!("[{}]", printer::print(value, &self.current_chunk.symbols));
        }
        println!("")
    }
}
