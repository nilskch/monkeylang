use compiler::parser::Parser;
use std::fs;
use std::process;

pub fn fmt(file_name: &String) {
    let source_code = match fs::read_to_string(file_name) {
        Ok(source_code) => source_code,
        Err(_) => {
            println!("ERROR: Can't open file '{file_name}'.");
            process::exit(1);
        }
    };

    let mut parser = Parser::new(source_code);
    let program = match parser.parse_program() {
        Ok(program) => program,
        Err(err) => {
            println!("{err}");
            process::exit(1);
        }
    };

    let formatted_code = program.format();
    if let Err(err) = fs::write(file_name, formatted_code) {
        println!("{err}");
        process::exit(1);
    }
}
