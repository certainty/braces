use crate::repl;
use crate::vm::VM;

pub struct Command {}

impl Command {
    pub fn new(_opts: &clap::ArgMatches) -> Self {
        Self {}
    }

    pub fn options<'a>() -> clap::Command<'a> {
        clap::Command::new("repl").alias("R").about("Run the REPL")
    }

    pub fn run(&self) -> anyhow::Result<()> {
        let vm = VM::default();
        let mut repl = repl::Repl::new(vm).unwrap();

        repl.run_loop()
    }
}
