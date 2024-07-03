use clap::{Arg, Command};

fn main() {
    let _m = Command::new("My Program")
        .author("Nils Koch, mail@nilskch.dev")
        .version("1.0.2")
        .about("Use the monkey cli to run, build, or format monkey code!")
        .arg(Arg::new("command").help("'run', 'build', 'fmt'."))
        .arg(Arg::new("file").help("The path to the .mky file."))
        .after_help(
            "Longer explanation to appear after the options when \
                     displaying the help information from --help or -h",
        )
        .get_matches();
}
