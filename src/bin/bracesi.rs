use braces::vm;
use braces::vm::BracesVM;
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

    let mut vm = vm::default();

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match vm.run_string(&line) {
                    Ok(Some(v)) => println!("{:?}", vm.print(v)),
                    Ok(_) => (),
                    Err(e) => eprintln!("Error: {}", e),
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
