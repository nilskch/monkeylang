mod fmt;
mod run;

use clap::{Arg, ArgAction, Command};

fn main() {
    let matches = Command::new("monkey")
        .author("Nils Koch, mail@nilskch.dev")
        .version("1.0.2")
        .about("Use the monkey cli to run or format monkey code!")
        .subcommand(
            Command::new("run")
                .about("Run monkey code from a file")
                .arg(
                    Arg::new("file")
                        .help("Path to the .mky file.")
                        .action(ArgAction::Set),
                ),
        )
        .subcommand(
            Command::new("fmt")
                .about("Format monkey code in a file")
                .arg(
                    Arg::new("file")
                        .help("Path to the .mky file.")
                        .action(ArgAction::Set),
                ),
        )
        .after_help(
            "Checkout the monkey playground (https://nilskch.github.io/monkeylang/) for example code of the monkey programming language!",
        )
        .get_matches();

    if !matches.args_present() {
        println!("Welcome to the Monkey Programming Language!");
        return compiler::repl::start();
    }
}
