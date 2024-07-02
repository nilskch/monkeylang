use clap::{Arg, Command};

fn main() {
    let _m = Command::new("My Program")
        .author("Nils Koch, mail@nilskch.dev")
        .version("1.0.2")
        .about("Explains in brief what the program does")
        .arg(Arg::new("in_file"))
        .after_help(
            "Longer explanation to appear after the options when \
                     displaying the help information from --help or -h",
        )
        .get_matches();
}
