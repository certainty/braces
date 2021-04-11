use braces::compiler::error::UserMessage;
use braces::vm;
use braces::vm::VM;
use rustyline::error::ReadlineError;
use rustyline::Editor;

fn main() {
    pretty_env_logger::init();
    repl();
}

fn repl() {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    let mut vm = VM::new();

    loop {
        println!("{:#?}", vm);
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match vm.run_string(&line, "repl") {
                    Ok(v) => {
                        println!("{:#?}", vm);
                        println!("{}", vm.write(&v))
                    }
                    Err(vm::Error::CompilerError(e)) => e.print_user_friendly_message(),
                    Err(e @ vm::Error::RuntimeError(_, _)) => eprintln!("{}", e),
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
