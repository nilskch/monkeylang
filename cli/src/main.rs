use clap::{Arg, Command};

fn main() {
    let cli = Command::new("monkey")
        .author("Nils Koch, mail@nilskch.dev")
        .version("1.0.2")
        .about("Use the monkey cli to run or format monkey code!")
        .arg(Arg::new("command").help("'run' or 'fmt'."))
        .arg(Arg::new("file").help("The path to the .mky file."))
        .get_matches();

    if !cli.args_present() {
        println!("Welcome to the Monkey Programming Language!");
        return compiler::repl::start();
    }

    // TODO: handle 'run' and 'fmt' subcommands
}
