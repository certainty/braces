use braces::cmd::compile;
use braces::cmd::repl;
use braces::cmd::run;
use clap::{AppSettings, Clap};

#[derive(Clap)]
#[clap(setting = AppSettings::ColoredHelp)]
struct Opts {
    #[clap(subcommand)]
    subcmd: SubCommand,
}

#[derive(Clap)]
enum SubCommand {
    #[clap(version = "0.1", author = "David K.")]
    Repl(repl::Opts),
    #[clap(version = "0.1", author = "David K.")]
    Compile(compile::Opts),
    Run(run::Opts),
}

fn main() {
    pretty_env_logger::init();
    let opts: Opts = Opts::parse();

    match opts.subcmd {
        SubCommand::Repl(opts) => repl::execute(&opts),
        SubCommand::Compile(opts) => compile::execute(&opts),
        SubCommand::Run(opts) => run::execute(&opts),
    }
}
