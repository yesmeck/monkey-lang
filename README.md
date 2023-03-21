# Monkey

A [Monkey](https://monkeylang.org/) implementation in Rust for learning purpose.

This impelmentation includes both an [interpreter](https://github.com/yesmeck/monkey-lang/blob/master/src/evaluator.rs) and a [virtual machine](https://github.com/yesmeck/monkey-lang/blob/master/src/vm.rs).

## Usage

### Virtual Machine

To run the Virtual Machine, use the following command:

```bash
cargo run
```

This will start a REPL where you can enter Monkey code.

Example:

```bash
>> let a = 5;
null
>> let b = 10;
null
>> let add = fn(x, y) { x + y };
null
>> add(a, b);
15
```

You can also run a Monkey file with this command:

```
cargo run -- sample.mk
```

### Interpreter

To run the interpreter, use the following command:

```bash
cargo run --engine=eval
```

## License

[MIT](LICENSE)
