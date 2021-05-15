use crate::repl;
use crate::vm::VM;
use clap::Clap;

#[derive(Clap)]
#[clap(version = "0.1", author = "David K.", about = "Start the REPL")]
pub struct Opts {}

pub fn execute(_opts: &Opts) {
    let vm = VM::default();
    let mut repl = repl::Repl::new(vm).unwrap();

    repl.run_loop().unwrap();
}
