use clap::Clap;

#[derive(Clap)]
pub struct Opts {}

pub fn execute(_opts: &Opts) -> anyhow::Result<()> {
    Ok(())
}
