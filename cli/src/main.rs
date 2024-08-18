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
                        .help("Path to the .mky file")
                        .action(ArgAction::Set)
                        .required(true),
                ),
        )
        .subcommand(
            Command::new("fmt")
                .about("Format monkey code in a file")
                .arg(
                    Arg::new("file")
                        .help("Path to the .mky file")
                        .action(ArgAction::Set)
                        .required(true),
                ),
        )
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("run") {
        let file_name = matches.get_one::<String>("file").unwrap();
        return run::run(file_name);
    }

    if let Some(matches) = matches.subcommand_matches("fmt") {
        let file_name = matches.get_one::<String>("file").unwrap();
        return fmt::fmt(file_name);
    }

    compiler::repl::start();
}
