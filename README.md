# Monkeylang

This is a Rust implementation of the Monkey programming language created by [Thorsten Ball](https://github.com/mrnugget).
This is a fun project to learn more about interpreters and compilers and to learn Rust programming language.
The interpreter is written in pure Rust without any external libraries.

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
```

### Usage
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
- THE Rust book "[The Rust Programming Language](https://doc.rust-lang.org/book/)"
- The book "[Rust for Rustaceans](https://rust-for-rustaceans.com/)" by [Jon Gjengset](https://github.com/jonhoo)
