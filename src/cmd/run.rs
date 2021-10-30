use crate::compiler::source::FileSource;
use crate::compiler::Compiler;
use crate::vm::VM;
use clap::Parser;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[clap(
    version = "0.1",
    author = "David K.",
    about = "Run the file specified by <input>"
)]
pub struct Opts {
    input: String,
}

pub fn execute(opts: &Opts) -> anyhow::Result<()> {
    let mut vm = VM::default();
    let mut source = FileSource::new(PathBuf::from(opts.input.clone()));
    let mut compiler = Compiler::new();

    match compiler.compile(&mut source) {
        Ok(unit) => match vm.interpret(unit) {
            Ok(_) => (),
            Err(e) => vm.print_error(&e, &compiler),
        },
        Err(e) => compiler.print_error(&e),
    }

    Ok(())
}
