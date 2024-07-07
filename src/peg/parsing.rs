use crate::peg::expression::Expression;
use std::fmt::{Display, Formatter};

/// Capture(a, b) is an index into the source string x representing the slice x\[a..b]
#[derive(Clone, Debug)]
pub struct Capture(pub usize, pub usize);

#[derive(Clone, Debug)]
pub enum Token {
    Terminal(Capture),
    NonTerminal(String, Capture, Vec<Token>),
}

#[derive(Clone, Debug)]
pub struct ParserOutput(pub u32, pub usize, pub Result<Vec<Token>, ParserError>);

#[derive(Clone, Debug)]
#[allow(unused)]
pub enum ParserErrorCode {
    UnexpectedEndOfInput,
    ExpressionDoesNotMatch,
    NotDidMatch(Vec<Token>),
    NonTerminalDoesNotMatch,
}

#[derive(Clone, Debug)]
#[allow(unused)]
pub struct ParserError {
    pub position: usize,
    pub expression: Expression,
    pub error: ParserErrorCode,
    pub cause: Option<Box<ParserError>>,
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.cause {
            None => write!(
                f,
                "Encountered {} @ {} for '{}'",
                self.error, self.position, self.expression
            ),
            Some(inner) => write!(
                f,
                "Encountered {} @ {} for '{}'\n\tCaused by: {}",
                self.error, self.position, self.expression, inner
            ),
        }
    }
}

impl Display for ParserErrorCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
