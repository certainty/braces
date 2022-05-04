pub struct Command {}

impl Command {
    pub fn new(_opts: &clap::ArgMatches) -> Self {
        Self {}
    }

    pub fn options<'a>() -> clap::Command<'a> {
        clap::Command::new("compile").about("compile files")
    }
    pub fn run(&self) -> anyhow::Result<()> {
        Ok(())
    }
}
