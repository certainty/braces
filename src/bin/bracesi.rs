use braces::vm;
use braces::vm::printer;
use rustyline::error::ReadlineError;
use rustyline::Editor;

fn main() {
    pretty_env_logger::init();
    let vm = vm::default();
    repl(&vm);
}

fn repl(instance: &impl vm::BracesVM) {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match instance.run_string(&line) {
                    Ok(Some(v)) => println!("{:?}", &v),
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
