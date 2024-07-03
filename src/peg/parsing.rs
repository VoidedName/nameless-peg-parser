/// Capture(a, b) is an index into the source string x representing the slice x\[a..b]
#[derive(Clone, Debug)]
pub struct Capture(pub usize, pub usize);

#[derive(Clone, Debug)]
pub enum Token {
    Terminal(Capture),
    NonTerminal(String, Capture, Vec<Token>),
}

#[derive(Clone, Debug)]
pub struct ParserOutput(pub u32, pub usize, pub Result<Vec<Token>, ()>);
