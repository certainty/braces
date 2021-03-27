use braces::compiler;
use braces::vm::disassembler::disassemble;
use braces::vm::printer;
use braces::vm::stack_vm;
use braces::vm::stack_vm::StackVM;
use braces::vm::value;
use compiler::source;
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

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match interprete(&line) {
                    Ok(Some(v)) => println!("{}", printer::print(&v)),
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

fn interprete(line: &str) -> stack_vm::Result<Option<value::Value>> {
    let mut source: source::StringSource = line.into();
    if let Some(chunk) = compiler::jit_compile(&mut source)? {
        disassemble(&mut std::io::stdout(), &chunk, "REPL");
        StackVM::interprete(&chunk)
    } else {
        Ok(None)
    }
}
