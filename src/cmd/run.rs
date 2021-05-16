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

pub fn execute(opts: &Opts) -> anyhow::Result<()> {
    let mut vm = VM::default();

    match vm.run_file(std::path::PathBuf::from(opts.input.clone())) {
        Ok(_) => (),
        Err(vm::Error::CompilerError(e)) => e.print_user_friendly_message(),
        Err(vm::Error::RuntimeError(msg, line, stack_trace)) => {
            eprintln!("{}:{}\n{}", msg, line, stack_trace.as_string())
        }
        Err(e @ vm::Error::CompilerBug(_)) => eprintln!("{}", e),
    }
    Ok(())
}
