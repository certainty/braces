use crate::compiler::source::FileSource;
use crate::compiler::Compiler;
use clap::arg;
use std::io::{self, Write};
use std::path::PathBuf;

pub struct Command {
    input: PathBuf,
}

impl Command {
    pub fn new(opts: &clap::ArgMatches) -> Self {
        let path = opts.value_of("INPUT").unwrap();

        Self {
            input: PathBuf::from(path),
        }
    }

    pub fn options<'a>() -> clap::Command<'a> {
        clap::Command::new("compile")
            .alias("c")
            .about("compile the specified file to rust")
            .arg(arg!([INPUT]))
    }

    pub fn run(&self) -> anyhow::Result<()> {
        let mut source = FileSource::new(self.input.clone());
        let mut compiler = Compiler::new();

        match compiler.compile_rust(&mut source) {
            Ok(rust_source) => {
                println!("out.rs");
                io::stdout().write_all(rust_source.as_bytes())?;
            }
            Err(e) => compiler.print_error(&e),
        }

        Ok(())
    }
}
