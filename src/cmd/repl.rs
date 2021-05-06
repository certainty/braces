use crate::compiler::error::UserMessage;
use crate::vm;
use crate::vm::scheme::value::Value;
use crate::vm::VM;
use clap::Clap;
use rustyline::error::ReadlineError;
use rustyline::Editor;

#[derive(Clap)]
#[clap(version = "0.1", author = "David K.", about = "Start the REPL")]
pub struct Opts {}

pub fn execute(_opts: &Opts) {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    let mut vm = VM::default();

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match vm.run_string(&line, "repl") {
                    Ok(Value::Unspecified) => (),
                    Ok(v) => println!("{}", vm.write(&v)),
                    Err(vm::Error::CompilerError(e)) => e.print_user_friendly_message(),
                    Err(e @ vm::Error::RuntimeError(_, _)) => eprintln!("{} ", e),
                    Err(e @ vm::Error::CompilerBug(_)) => eprintln!("{}", e),
                };
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("history.txt").unwrap();
}
