use std::fs::create_dir;
#[allow(unused)]
#[allow(unused)]
use std::fs::File;
#[allow(unused)]
use std::io::Write;
#[allow(unused)]
use log::{LevelFilter};
use crate::example::SIMPLE_LANGUAGE;
use crate::peg::grammar::PEG;
#[allow(unused)]
use crate::peg::transformer::Transformer;

mod peg;
mod example;

/// ```
/// PEG = (NT, T, R, S)
///     NT = Set of NonTerminals
///     T = Set of Terminals // may not be necessary to define in practice,
///                             since it's just "all characters" (ASCII) unless we want to restrict it
///     R = Rules i.e. R(NT) -> e with e = parsing expression
///     S = Start Symbol i.e. S elem NT
///
/// Parser Input: ASCII String
/// Parser Output: List of captured tokens. Terminals do not contain other tokens, Non Terminals are a list of tokens
///
/// parse(rule, text) => (cost, cursor, Result<Token, ()>) // Should Error contain more information?
/// cursor = new position in input text
/// Token = NonTerminal(Capture, Tokens) | Terminal(Capture)
/// Capture = (start, end) | index into the input string or should this just be the input string? using slices this would
///                          probably work without immense storage costs
///
/// Parsing expression
/// if e, e1 and e2 is a parsing expression then the following are also parsing expressions
/// - e1e2 | sequence
/// - e1/e2 | prio choice
/// - !e | not predicate
/// - &e | and predicate
/// - (e) | group
/// - e* | zero or more
/// - e+ | one or more (same as ee*)
/// - 'x' with x elem T^* | literal
/// - "x" with x elem T^* | literal
/// - [xyz] with x,y,z elem T | class
/// - [x-z] with x,z elem T | range
/// - . | any
/// - empty | the empty string
///
/// Parsing Rules
/// EMPTY:
///     parse(empty, x) => (1, x, Ok(empty))
/// ANY:
///     parse(any, xy) => (1, y, Ok(x)) if x elem T
///     parse(any, x) => (1, x, Err(())) if x is empty
/// RANGE:
///     parse(range(a, b), xy) => (1, y, Ok(x)) if x elem T and a <= x <= b (in ASCII)
///     parse(range(a, b), x) => (1, x, Err(())) if x is empty
/// CLASS:
///     parse(class(s), xy) => (1, y, Ok(x)) if s subset of T^* and x elem s
///     parse(class(s), x) => (1, x, Err(())) if s subset of T^* and x not elem s
/// LITERAL || define recursive?
///     parse(literal(x), xy) => (len(x) + 1, y, Ok(x)) with x elem T^*
///     parse(literal(x), y) => (len(nr of matches + 1), y, Err(())) with x elem T^* and x not prefix of y
/// ZERO OR MORE
///     parse(e*, xyz) => (n1+n2+1, z, Ok(xy)) if parse(e, xy) => (n1, y, Ok(x)) and parse(e*, yz) => (n2, z, Ok(y))
///     parse(e*, x) => (n1+1, x, Ok(empty)) if parse(e, x) => (n1, x, Err(()))
/// ONE OR MORE
///     parse(e+, x) => parse(ee*, x)
/// GROUP
///     parse((e), x) => parse(e, x)
/// AND
///     parse(&e, xy) => (n+1, x, Ok(empty)) if parse(e, xy) => (n, y, Ok(x))
///     parse(&e, xy) => (n+1, x, Err(())) if parse(e, xy) => (n, x, Err())
/// NOT
///     parse(!e, xy) => (n+1, x, Ok(empty)) if parse(e, xy) => (n, y, Err(x))
///     parse(!e, xy) => (n+1, x, Err(())) if parse(e, xy) => (n, x, Ok(x))
/// CHOICE
///     parse(e1/e1, xy) => (n+1, y, Ok(x)) if parse(e1, xy) => (n, y, Ok(x))
///     parse(e1/e1, xy) => (n1+n2+1, y, Ok(x)) if parse(e1, xy) => (n1, x, Err(())) and parse(e2, xy) => (n2, y, Ok(x))
///     parse(e1/e2, xy) => (n1+n2+1, x, Err(())) if neither e1 nor e2 parse xy
/// SEQUENCE
///     parse(e1e2, xyz) => (n1+n2+1, z, Ok(xy)) if parse(e1, xy) => (n1, y, Ok(x)) and parse(e2, yz) => (n2, z, Ok(y))
///     parse(e1e2, xyz) => (n1+1, x, Err(()) if e1 does not parse xyz
///     parse(e1e2, xyz) => (n1+n2+1, x, Err(()) if parse(e1, xy) => (n1, y, Ok(x)) and e2 does not parse xyz
///
/// ```

#[allow(unused)]
const PEG_GRAMMAR: &str = "\
AND <- ('&' Spacing)
CLOSE <- (')' Spacing)
Char <- (('\\\\' [\\]\\[\\\\t'nr\"]) / ('\\\\' [0-9]+ ';') / (!'\\\\' .))
Class <- ('[' (!']' ClassMember)* ']' Spacing)
ClassMember <- (Range / Char)
Comment <- ('#' (!EndOfLine .)* EndOfLine)
DOT <- ('.' Spacing)
Definition <- (Identifier LEFTARROW Expression)
EndOfFile <- !.
EndOfLine <- ('\\r\\n' / '\\r' / '\\n')
Expression <- (Sequence (SLASH Sequence)*)
Grammar <- (Spacing Definition* EndOfFile)
Identifier <- (([a-z] / [A-Z] / '_') ([a-z] / [A-Z] / '_' / [0-9])* Spacing)
LEFTARROW <- ('<-' Spacing)
Literal <- (('\\'' (!'\\'' Char)* '\\'' Spacing) / ('\"' (!'\"' Char)* '\"' Spacing))
NOT <- ('!' Spacing)
OPEN <- ('(' Spacing)
PLUS <- ('+' Spacing)
Prefix <- (AND / NOT)?
Primary <- ((Identifier !LEFTARROW) / (OPEN Expression CLOSE) / Literal / Class / DOT)
QUESTION <- ('?' Spacing)
Range <- (Char '-' Char)
SLASH <- ('/' Spacing)
STAR <- ('*' Spacing)
Sequence <- (Prefix Primary Suffix)*
Space <- (' ' / '\\t' / EndOfLine)
Spacing <- (Space / Comment)*
Suffix <- (QUESTION / STAR / PLUS)?";

fn main() {
    use env_logger;
    env_logger::builder().init();

    // let peg = PEG::from_grammar(PEG_GRAMMAR).unwrap();
    // let cst = peg.parse(PEG_GRAMMAR);
    //
    // let transformer = Transformer {
    //     input_string: PEG_GRAMMAR
    // };
    //
    // create_dir("./artifacts").unwrap();
    //
    // let mut file = File::create("./artifacts/peg.cst").unwrap();
    // write!(file, "{:#?}", cst).unwrap();
    //
    // let parsed_peg = transformer.transform(cst.2.unwrap()).unwrap();
    // let mut file = File::create("./artifacts/peg.grammar").unwrap();
    // write!(file, "{}", parsed_peg).unwrap();
    //
    // let mut file = File::create("./artifacts/peg.ast").unwrap();
    // write!(file, "{:#?}", parsed_peg).unwrap();

    let p = PEG::from_grammar(SIMPLE_LANGUAGE).unwrap();
    println!("{:#?}", p.parse("aabbccc"));
    println!("{}", p);
}
