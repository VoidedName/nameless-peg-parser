# Nameless PEG Parser
This is an implementation of https://bford.info/pub/lang/peg.pdf

I implemented it mostly out of academic curiosity and it is not optimized in any way shape or form.

There are two examples included in the examples folder, one for success, one for failure.

The parser logs to debug! and error! or the log module, if you want to see some logs, include your logging implementation and enable the appropriate log level. The examples include env_logger and logging can be enabled by setting the env variable "RUST_LOG=debug".

Usage:

```rust
let parser = PEG::from_grammar(start_rule, grammar_string)?;
let result = parser.parse(input_string);
```

Feel free to make PRs :), I will most likely continue tinkering with it and maybe even properly optimize it.


Can be found as a crate: https://docs.rs/nameless-peg-parser/latest/nameless_peg_parser/index.html