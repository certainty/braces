use crate::compiler::source::FileSource;
use crate::compiler::Compiler;
use crate::vm::VM;
use clap::arg;
use std::path::PathBuf;

pub struct Command {
    input: PathBuf,
}

impl Command {
    pub fn new(opts: &clap::ArgMatches) -> Self {
        let path = opts.value_of("input").unwrap();

        Self {
            input: PathBuf::from(path),
        }
    }

    pub fn options<'a>() -> clap::Command<'a> {
        clap::Command::new("run")
            .alias("r")
            .about("run the specified file")
            .arg(arg!(input: [INPUT]))
    }

    pub fn run(&self) -> anyhow::Result<()> {
        let mut vm = VM::default();
        let mut source = FileSource::new(self.input.clone());
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
}
