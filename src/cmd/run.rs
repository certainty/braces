use crate::compiler::error::UserMessage;
use crate::vm;
use crate::vm::VM;
use clap::Clap;

#[derive(Clap, Debug)]
#[clap(
    version = "0.1",
    author = "David K.",
    about = "Run the file specified by <input>"
)]
pub struct Opts {
    input: String,
}

pub fn execute(opts: &Opts) {
    let mut vm = VM::default();

    match vm.run_file(std::path::PathBuf::from(opts.input.clone())) {
        Ok(_) => (),
        Err(vm::Error::CompilerError(e)) => e.print_user_friendly_message(),
        Err(e @ vm::Error::RuntimeError(_, _)) => eprintln!("{}", e),
        Err(e @ vm::Error::CompilerBug(_)) => eprintln!("{}", e),
    }
}
