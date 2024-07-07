# Nameless PEG Parser
This is an implementation of https://bford.info/pub/lang/peg.pdf

I implemented it mostly out of academic curiosity and it is not optimized in any way shape or form.

There are two examples included in the examples folder, one for success, one for failure.

The parser logs to debug! and error! of the log module, if you want to see some logs, include your logging implementation and enable the appropriate log level. The examples include env_logger and logging can be enabled by setting the env variable "RUST_LOG=debug".

The output is a vector of tokens with capture indexes into the original input string. This is a deviation from the paper, since the paper suggests returning the actual string, but this would be rather unwieldy, although one could return string slices at no real cost (except the lifetime restriction). I may include a transformer that does this.

Usage:

```rust
use nameless_peg_parser::peg::grammar::PEG;

fn main() {
    let parser = PEG::from_grammar(start_rule, grammar_string).unwrap();
    let result = parser.parse(input_string);
    println!("{:?}", result); // ParserOutput(Cost, Position, Result<Vec<Token>>,ParserError>)
}
```

Feel free to make PRs :), I will most likely continue tinkering with it and maybe even properly optimize it.

YT: https://www.youtube.com/watch?v=awtHSLEIFxA

Can be found as a crate: https://docs.rs/nameless-peg-parser/latest/nameless_peg_parser/index.html