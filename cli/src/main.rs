use clap::{Arg, ArgAction, Command};

fn main() {
    let matches = Command::new("monkey")
        .author("Nils Koch, mail@nilskch.dev")
        .version("1.0.2")
        .about("Use the monkey cli to run or format monkey code!")
        .arg(
            Arg::new("cmd")
                .help("'run' or 'fmt'.")
                .action(ArgAction::Set),
        )
        .arg(
            Arg::new("file")
                .help("Path to the .mky file.")
                .action(ArgAction::Set),
        )
        .get_matches();

    if !matches.args_present() {
        println!("Welcome to the Monkey Programming Language!");
        return compiler::repl::start();
    }

    let _file_path = match matches.get_one::<&str>("file") {
        Some(file_path) => *file_path,
        None => panic!("No file path provided."),
    };

    match *matches.get_one::<&str>("cmd").unwrap() {
        "run" => println!("RUN"),
        "fmt" => println!("FMT"),
        _ => println!("ERROR"),
    }
}
