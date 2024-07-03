use crate::peg::grammar::PEG;
use crate::peg::parsing::{Capture, ParserOutput, Token};
use log::debug;
use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

// might need inner structs
#[derive(Clone, Debug)]
pub enum Expression {
    Empty,
    Any,
    Literal(String),
    NonTerminal(String),   // references a rule
    Range(String, String), // restrict to ASCII only, don't want to deal with "characters"
    Class(HashSet<String>),
    Group(Rc<Box<Expression>>),
    ZeroOrMore(Rc<Box<Expression>>),
    OneOrMore(Rc<Box<Expression>>),
    Optional(Rc<Box<Expression>>),
    And(Rc<Box<Expression>>),
    Not(Rc<Box<Expression>>),
    Choice(Vec<Expression>), // generalizing it to any number of choices | illegal to construct with less than 2
    Sequence(Vec<Expression>), // generalizing it to any number of sequences | illegal to be empty
}

impl Expression {
    pub fn into_rc_box(self) -> Rc<Box<Expression>> {
        Rc::new(Box::new(self))
    }

    pub fn make_class(symbols: &[&str]) -> Expression {
        let mut class: HashSet<String> = Default::default();
        for s in symbols.iter().map(|c| c.to_string()) {
            class.insert(s);
        }

        Expression::Class(class)
    }

    pub fn parse(&self, peg: &PEG, input: &str, cursor: usize, depth: usize) -> ParserOutput {
        match self {
            Expression::Empty => ParserOutput(1, cursor, Ok(vec![])),
            Expression::Any => {
                if cursor < input.len() {
                    ParserOutput(
                        1,
                        cursor + 1,
                        Ok(vec![Token::Terminal(Capture(cursor, cursor + 1))]),
                    )
                } else {
                    ParserOutput(1, cursor, Err(()))
                }
            }
            Expression::Literal(pattern) => {
                let input = &input[cursor..];
                let len = pattern.len();
                if input.starts_with(pattern) {
                    ParserOutput(
                        len as u32,
                        cursor + len,
                        Ok(vec![Token::Terminal(Capture(cursor, cursor + len))]),
                    )
                } else {
                    // eh, not quite what I wrote above, but close enough (cost is potentially too large)...
                    ParserOutput(len as u32, cursor, Err(()))
                }
            }
            Expression::NonTerminal(nt) => {
                let rule = peg
                    .rules
                    .get(nt)
                    .expect(format!("Encountered unknown NonTerminal symbol: {}", nt).as_str());

                debug!("{}parsing {nt} @ {cursor}: {rule}", "│".repeat(depth));

                let ParserOutput(cost, new_cursor, res) = rule.parse(peg, input, cursor, depth + 1);

                let res = match res {
                    Ok(matches) => ParserOutput(
                        cost,
                        new_cursor,
                        Ok(vec![Token::NonTerminal(
                            nt.clone(),
                            Capture(cursor, new_cursor),
                            matches,
                        )]),
                    ),
                    Err(_) => ParserOutput(cost, cursor, Err(())),
                };

                debug!(
                    "{}└done parsing {nt} @ {cursor} advanced to {new_cursor} with '{}'",
                    "│".repeat(depth),
                    if res.2.is_ok() { "success" } else { "failure" }
                );

                res
            }
            Expression::Range(from, to) => {
                // test this!
                if (from.as_str()..to.as_str()).contains(&&input[cursor..]) {
                    // per paper this should have been desugared into a choice and the cost would depend on
                    // the ordering in the choice, but this can be done in constant time...
                    ParserOutput(
                        1,
                        cursor + 1,
                        Ok(vec![Token::Terminal(Capture(cursor, cursor + 1))]),
                    )
                } else {
                    ParserOutput(1, cursor, Err(()))
                }
            }
            Expression::Class(class) => {
                let input = &input[cursor..];
                let mut cost = 1;
                for symbol in class {
                    cost += symbol.len();
                    if input.starts_with(symbol) {
                        return ParserOutput(
                            cost as u32,
                            cursor + symbol.len(),
                            Ok(vec![Token::Terminal(Capture(
                                cursor,
                                cursor + symbol.len(),
                            ))]),
                        );
                    }
                }
                ParserOutput(cost as u32, cursor, Err(()))
            }
            Expression::Group(expression) => {
                let ParserOutput(cost, cursor, res) = expression.parse(peg, input, cursor, depth);
                ParserOutput(cost + 1, cursor, res)
            }
            Expression::ZeroOrMore(expression) => {
                let mut captures = vec![];
                let mut new_cursor = cursor;
                let mut running_cost = 1;
                loop {
                    let ParserOutput(cost, downstream_cursor, res) =
                        expression.parse(peg, input, new_cursor, depth);
                    running_cost += cost;
                    match res {
                        Ok(mut matches) => {
                            new_cursor = downstream_cursor;
                            captures.append(&mut matches);
                        }
                        Err(_) => {
                            break ParserOutput(running_cost, new_cursor, Ok(captures));
                        }
                    }
                }
            }
            Expression::OneOrMore(expression) => {
                let desugared = Expression::Sequence(vec![
                    Expression::Group(expression.clone()),
                    Expression::ZeroOrMore(expression.clone()),
                ]);
                desugared.parse(peg, input, cursor, depth)
            }
            Expression::Optional(expression) => {
                let ParserOutput(cost, cursor, res) = expression.parse(peg, input, cursor, depth);
                match res {
                    Ok(matches) => ParserOutput(cost + 1, cursor, Ok(matches)),
                    Err(_) => ParserOutput(cost + 1, cursor, Ok(vec![])),
                }
            }
            Expression::And(peek) => {
                let ParserOutput(cost, _, res) = peek.parse(peg, input, cursor, depth);
                match res {
                    Ok(_) => ParserOutput(cost + 1, cursor, Ok(vec![])),
                    Err(_) => ParserOutput(cost + 1, cursor, Err(())),
                }
            }
            Expression::Not(peek) => {
                let ParserOutput(cost, _, res) = peek.parse(peg, input, cursor, depth);
                match res {
                    Ok(_) => ParserOutput(cost + 1, cursor, Err(())),
                    Err(_) => ParserOutput(cost + 1, cursor, Ok(vec![])),
                }
            }
            Expression::Choice(choices) => {
                let mut running_cost = 1;
                for expression in choices {
                    let ParserOutput(cost, cursor, res) =
                        expression.parse(peg, input, cursor, depth);
                    running_cost += cost;
                    if let Ok(res) = res {
                        return ParserOutput(running_cost, cursor, Ok(res));
                    }
                }

                ParserOutput(running_cost, cursor, Err(()))
            }
            Expression::Sequence(sequence) => {
                let mut running_cost = 1;
                let mut captures: Vec<Token> = vec![];
                let mut new_cursor = cursor;

                for expression in sequence {
                    let ParserOutput(cost, cursor, res) =
                        expression.parse(peg, input, new_cursor, depth);
                    running_cost += cost;
                    if let Ok(mut res) = res {
                        captures.append(&mut res);
                        new_cursor = cursor;
                    } else {
                        return ParserOutput(running_cost, cursor, Err(()));
                    }
                }

                ParserOutput(running_cost, new_cursor, Ok(captures))
            }
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Empty => write!(f, ""),
            Expression::Any => write!(f, "."),
            Expression::Literal(literal) => write!(
                f,
                "'{}'",
                literal
                    .replace("\\", "\\\\")
                    .replace("\\\\0", "\\0")
                    .replace("\\\\1", "\\1")
                    .replace("\\\\2", "\\2")
                    .replace("\\\\3", "\\3")
                    .replace("\\\\4", "\\4")
                    .replace("\\\\5", "\\5")
                    .replace("\\\\6", "\\6")
                    .replace("\\\\7", "\\7")
                    .replace("\\\\8", "\\8")
                    .replace("\\\\9", "\\9")
                    .replace("'", "\\'")
                    .replace("\r", "\\r")
                    .replace("\t", "\\t")
                    .replace("\n", "\\n")
            ),
            Expression::NonTerminal(token) => write!(f, "{}", token),
            Expression::Range(a, b) => write!(f, "[{}-{}]", a, b), // there is no way to output [az-x] atm, just a way to parse it
            Expression::Class(class) => {
                write!(
                    f,
                    "[{}]",
                    class
                        .iter()
                        .map(|c| c
                            .replace("\\", "\\\\")
                            .replace("\\\\0", "\\0")
                            .replace("\\\\1", "\\1")
                            .replace("\\\\2", "\\2")
                            .replace("\\\\3", "\\3")
                            .replace("\\\\4", "\\4")
                            .replace("\\\\5", "\\5")
                            .replace("\\\\6", "\\6")
                            .replace("\\\\7", "\\7")
                            .replace("\\\\8", "\\8")
                            .replace("\\\\9", "\\9")
                            .replace("\r", "\\r")
                            .replace("\t", "\\t")
                            .replace("\n", "\\n")
                            .replace("[", "\\[")
                            .replace("]", "\\]"))
                        .collect::<Vec<_>>()
                        .join("")
                )
            }
            Expression::Group(group) => write!(f, "({})", group),
            Expression::ZeroOrMore(inner) => write!(f, "{}*", inner),
            Expression::OneOrMore(inner) => write!(f, "{}+", inner),
            Expression::Optional(inner) => write!(f, "{}?", inner),
            Expression::And(inner) => write!(f, "&{}", inner),
            Expression::Not(inner) => write!(f, "!{}", inner),
            Expression::Choice(choices) => {
                if choices.len() > 1 {
                    write!(
                        f,
                        "({})",
                        choices
                            .iter()
                            .map(|c| format!("{}", c))
                            .collect::<Vec<_>>()
                            .join(" / ")
                    )
                } else {
                    write!(
                        f,
                        "{}",
                        choices
                            .iter()
                            .map(|c| format!("{}", c))
                            .collect::<Vec<_>>()
                            .join(" / ")
                    )
                }
            }
            Expression::Sequence(sequence) => {
                if sequence.len() > 1 {
                    write!(
                        f,
                        "({})",
                        sequence
                            .iter()
                            .map(|c| format!("{}", c))
                            .collect::<Vec<_>>()
                            .join(" ")
                    )
                } else {
                    write!(
                        f,
                        "{}",
                        sequence
                            .iter()
                            .map(|c| format!("{}", c))
                            .collect::<Vec<_>>()
                            .join(" ")
                    )
                }
            }
        }
    }
}
