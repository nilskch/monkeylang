# Monkeylang

This is a Rust implementation of the Monkey programming language created by [Thorsten Ball](https://github.com/mrnugget).
This is a fun project to learn more about interpreters and compilers and to learn Rust programming language.
The interpreter is written in pure Rust [without any external libraries](https://github.com/nilskch/monkeylang/blob/main/compiler/Cargo.toml).

At the moment this is "just" a tree-walking interpreter. I currently work on a bytecode compiler and a virtual machine to make the interpreter faster
(hence I called the crate "compiler" and not "interpreter" before you nitpickers call me out).

## Playground
Run monkey code and discover all language features online in the [wasm based playground](https://nilskch.github.io/monkeylang).
There are plenty examples that showcase all language features.

## CLI
You can use the monkey CLI to run monkey code locally. You can install it using cargo.

### Install
Install the monkey cli with cargo:
```
$ git clone https://github.com/nilskch/monkeylang.git && cd monkeylang
$ cargo install --path cli
$ monkey --help
Use the monkey cli to run or format monkey code!

Usage: monkey [COMMAND]

Commands:
  run   Run monkey code from a file
  fmt   Format monkey code in a file
  help  Print this message or the help of the given subcommand(s)

Options:
  -h, --help     Print help
  -V, --version  Print version
```

### Usage
Start the monkey repl:
```
$ monkey
Welcome to the Monkey Programming Language!
>> print("Hello World")
"Hello World"
>>
```

Run monkey code:
```
$ monkey run main.mky
```

Format monkey code:
```
$ monkey fmt main.mky
```

## Resources
I used the following resources for this project:
- The book "[Writing An Interpreter In Go](https://interpreterbook.com/)" by [Thorsten Ball](https://github.com/mrnugget)
- The sequel "[Writing A Compiler In Go](https://compilerbook.com/)" by [Thorsten Ball](https://github.com/mrnugget)
- THE Rust book "[The Rust Programming Language](https://doc.rust-lang.org/book/)" by [Steve Klabnik](https://github.com/steveklabnik) and [Carol Nichols](https://github.com/carols10cents)
- The book "[Rust for Rustaceans](https://rust-for-rustaceans.com/)" by [Jon Gjengset](https://github.com/jonhoo)
