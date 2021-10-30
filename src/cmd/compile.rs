use clap::Parser;

#[derive(Parser)]
pub struct Opts {}

pub fn execute(_opts: &Opts) -> anyhow::Result<()> {
    Ok(())
}
