use crate::compiler;
use crate::compiler::source::*;
use clap::Clap;

#[derive(Clap, Debug)]
#[clap(version = "0.1", author = "David K.", about = "compile <input>")]
pub struct Opts {
    input: String,
}

pub fn execute(opts: &Opts) -> anyhow::Result<()> {
    let mut compiler = compiler::Compiler::new();
    let file_path = std::path::PathBuf::from(opts.input.clone());
    let mut source = FileSource::new(file_path);
    let output = compiler.compile_program(&mut source)?;

    println!("{:#?}", output);

    Ok(())
}
