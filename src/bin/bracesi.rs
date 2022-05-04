extern crate clap;
use braces::cmd::compile;
use braces::cmd::repl;
use braces::cmd::run;
use braces::BRACES_VERSION;
fn main() {
    pretty_env_logger::init();

    match run() {
        Ok(()) => std::process::exit(0),
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1)
        }
    }
}

fn run() -> anyhow::Result<()> {
    let opts = clap::Command::new("bracesi")
        .version(BRACES_VERSION)
        .author("David K.")
        .about("A compiler and virtual machine for r7rs scheme")
        .infer_subcommands(true)
        .subcommand_required(true)
        .subcommand(repl::Command::options())
        .subcommand(compile::Command::options())
        .subcommand(run::Command::options())
        .get_matches();

    match opts.subcommand().unwrap() {
        ("repl", repl_opts) => Ok(repl::Command::new(&repl_opts).run()?),
        ("compile", compile_opts) => Ok(compile::Command::new(&compile_opts).run()?),
        ("run", run_opts) => Ok(run::Command::new(&run_opts).run()?),
        _ => unreachable!(),
    }
}
