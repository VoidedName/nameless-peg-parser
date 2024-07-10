use crate::peg::expression::Expression;
use crate::peg::expression::Expression::Literal;
use crate::peg::grammar::{
    AND_STR, CHAR_STR, CLASS_MEMBER_STR, CLASS_STR, CLOSE_STR, DEFINITION_STR, DOT_STR,
    END_OF_FILE_STR, EXPRESSION_STR, GRAMMAR_STR, IDENTIFIER_STR, LEFT_ARROW_STR, LITERAL_STR,
    NOT_STR, OPEN_STR, PEG, PLUS_STR, PREFIX_STR, PRIMARY_STR, QUESTION_STR, RANGE_STR,
    SEQUENCE_STR, SLASH_STR, SPACING_STR, STAR_STR, SUFFIX_STR,
};
use crate::peg::parsing::{Capture, Token};
use crate::peg::transformer::TransformError::{
    AmbiguousNonTerminal, CstShouldOnlyHaveOneRoot, CstShouldStartWithGrammar, EmptyIdentifier,
    UnExpectedToken, WrongNumberOfTokens,
};
use std::collections::{HashMap, HashSet};
use std::result;
use std::slice::Iter;

type Result<T> = result::Result<T, TransformError>;

// rethink error structure to be more useful, like also reporting cursor position
#[derive(Clone, Debug)]
pub enum TransformError {
    #[allow(unused)]
    CstShouldOnlyHaveOneRoot(String),
    #[allow(unused)]
    CstShouldStartWithGrammar(String),
    #[allow(unused)]
    UnExpectedToken(String),
    #[allow(unused)]
    AmbiguousNonTerminal(String),
    EmptyIdentifier,
    #[allow(unused)]
    WrongNumberOfTokens(String),
}

pub struct Transformer<'a> {
    pub input_string: &'a str,
}

impl Transformer<'_> {
    /// Transforms a CST of a PEG to a PEG, for the sake of this implementation we consider the
    /// token stream that was yielded from the parse as the CST
    pub fn transform(&self, start: &str, cst: Vec<Token>) -> Result<PEG> {
        // step one, turn cst into ast
        // step two analyse and make basic semantic checks
        //      a valid peg has a rule for every NonTerminal (on the reachable paths...)
        // step three, optimize to some degree?
        //      dissolve unnecessary groups, "groups" are only necessary in the cst

        if cst.len() != 1 {
            return Err(CstShouldOnlyHaveOneRoot(format!(
                "Found {} roots!",
                cst.len()
            )));
        }

        let root = cst.into_iter().next().unwrap();

        match root {
            Token::Terminal(_) => Err(CstShouldStartWithGrammar(
                "But found a Terminal Token instead!".to_string(),
            )),
            Token::NonTerminal(name, _, tokens) => {
                if GRAMMAR_STR != name {
                    Err(CstShouldStartWithGrammar(format!(
                        "But found a Non Terminal Token with name '{}' instead!",
                        name
                    )))
                } else {
                    let rules = self.process_grammar(tokens)?;
                    Ok(PEG {
                        rules,
                        start: start.to_string(),
                    })
                }
            }
        }
    }

    fn process_grammar(&self, tokens: Vec<Token>) -> Result<HashMap<String, Expression>> {
        // we expect the shape [spacing, definitions*, eof]
        let mut iter = tokens.into_iter();

        // check for leading space
        let spacing = iter.next().ok_or(UnExpectedToken(format!(
            "Expected NonTerminal '{SPACING_STR}'"
        )))?;

        if !Self::is_non_terminal(SPACING_STR, &spacing) {
            return Err(UnExpectedToken(format!(
                "Expected NonTerminal '{SPACING_STR}'"
            )));
        }

        let mut rules: HashMap<String, Expression> = Default::default();
        // process definitions
        let mut next = iter.next().ok_or(UnExpectedToken(format!(
            "Expected NonTerminal '{DEFINITION_STR}' or '{END_OF_FILE_STR}'"
        )))?;
        let mut non_terminals: HashSet<String> = Default::default(); // eh forgot the typing... goonna do it inefficiently for now
        loop {
            if Self::is_non_terminal(END_OF_FILE_STR, &next) {
                break;
            }
            // process definition

            let (name, expression, seen_non_terminals) = self.process_definition(&next)?;
            for non_terminal in seen_non_terminals {
                non_terminals.insert(non_terminal);
            }
            rules.insert(name, expression);

            next = iter.next().ok_or(UnExpectedToken(format!(
                "Expected NonTerminal '{DEFINITION_STR}' or '{END_OF_FILE_STR}'"
            )))?;
        }

        // check if fully consumed
        if iter.next().is_some() {
            return Err(UnExpectedToken(format!(
                "Grammar should have no token following {END_OF_FILE_STR}!"
            )));
        }

        // check if any observed non_terminals have no rule
        let keys = rules.keys().cloned().collect::<HashSet<String>>();
        let missing_keys = non_terminals
            .difference(&keys)
            .cloned()
            .collect::<Vec<_>>();

        if !missing_keys.is_empty() {
            return Err(AmbiguousNonTerminal(format!(
                "Missing rules for NonTerminals [{}]!",
                missing_keys.join(", ")
            )));
        }

        Ok(rules)
    }

    fn tokens_for_non_terminal<'a>(name: &str, token: &'a Token) -> Result<&'a Vec<Token>> {
        match token {
            Token::NonTerminal(n, _, tokens) => {
                if name == n {
                    Ok(tokens)
                } else {
                    Err(UnExpectedToken(format!(
                        "Expected NonTerminal '{}' but got '{}'",
                        name, n
                    )))
                }
            }
            _ => Err(UnExpectedToken(
                "Expected a NonTerminal but got terminal".to_string(),
            ))
        }
    }

    fn name_for_non_terminal(token: &Token) -> Result<String> {
        match token {
            Token::NonTerminal(name, _, _) => Ok(name.clone()),
            Token::Terminal(_) => Err(UnExpectedToken(
                "Expected a NonTerminal but got terminal".to_string(),
            )),
        }
    }

    fn expect_non_terminal(name: &str, token: &Token) -> Result<()> {
        let n = Self::name_for_non_terminal(token)?;
        if n == name {
            Ok(())
        } else {
            Err(UnExpectedToken(format!(
                "Expected NonTerminal '{}' but got '{}'",
                name, n
            )))
        }
    }

    fn is_non_terminal(name: &str, token: &Token) -> bool {
        match token {
            Token::NonTerminal(n, _, _) => name == n,
            _ => false,
        }
    }

    fn process_definition(&self, token: &Token) -> Result<(String, Expression, HashSet<String>)> {
        // expecting shape: Definition [ Identifier, LEFTARROW, Expression ]
        if let Token::NonTerminal(name, _, tokens) = token {
            if name != DEFINITION_STR {
                return Err(UnExpectedToken(format!(
                    "Expected NonTerminal '{DEFINITION_STR}'"
                )));
            }
            if tokens.len() != 3 {
                return Err(UnExpectedToken(format!(
                    "Expected Tokens '[ {IDENTIFIER_STR}, {LEFT_ARROW_STR}, {EXPRESSION_STR} ]'"
                )));
            }
            let [identifier, leftarrow, expression] = tokens.as_slice() else {
                panic!("Not Possible")
            };

            let identifier = self.process_identifier(identifier)?;
            if !Self::is_non_terminal(LEFT_ARROW_STR, leftarrow) {
                return Err(UnExpectedToken(format!(
                    "Expected Tokens '{LEFT_ARROW_STR}'"
                )));
            }
            let (expression, non_terminals) = self.process_expression(expression)?;

            Ok((identifier, expression, non_terminals))
        } else {
            Err(UnExpectedToken(format!(
                "Expected NonTerminal '{DEFINITION_STR}'"
            )))
        }
    }

    fn process_identifier(&self, token: &Token) -> Result<String> {
        // expected shape [ literal+, Spacing ]

        let mut identifier = String::new();
        let mut end = false;
        for token in Self::tokens_for_non_terminal(IDENTIFIER_STR, token)? {
            if end {
                return Err(UnExpectedToken(format!(
                    "Identifier should have no tokens after '{SPACING_STR}'"
                )));
            }
            if Self::is_non_terminal(SPACING_STR, token) {
                end = true;
                continue;
            }

            match token {
                Token::Terminal(Capture(from, to)) => {
                    identifier.push_str(&self.input_string[*from..*to]);
                }
                Token::NonTerminal(name, _, _) => {
                    return Err(UnExpectedToken(format!(
                        "Expected Terminal but got NonTerminal '{}'",
                        name
                    )))
                }
            }
        }

        if !identifier.is_empty() {
            Ok(identifier)
        } else {
            Err(EmptyIdentifier)
        }
    }

    fn process_expression(&self, token: &Token) -> Result<(Expression, HashSet<String>)> {
        // expected shape [Sequence, (SLASH Sequence)*]
        let mut sequences = vec![];
        let mut non_terminals = HashSet::new();

        let tokens = Self::tokens_for_non_terminal(EXPRESSION_STR, token)?;

        if tokens.len() % 2 != 1 {
            return Err(WrongNumberOfTokens(format!(
                "Expected odd number of tokens, but got {}",
                tokens.len()
            )));
        }

        let mut iter = tokens.iter();
        let (sequence, seen_non_terminals) = self.process_sequence(iter.next().unwrap())?;

        sequences.push(sequence);
        for non_terminal in seen_non_terminals {
            non_terminals.insert(non_terminal);
        }

        for [slash, sequence] in iter.chunk_fixed() {
            Self::expect_non_terminal(SLASH_STR, slash)?;

            let (sequence, seen_non_terminals) = self.process_sequence(sequence)?;

            sequences.push(sequence);
            for non_terminal in seen_non_terminals {
                non_terminals.insert(non_terminal);
            }
        }

        match sequences.len() {
            0 => Ok((Expression::Empty, non_terminals)),
            1 => Ok((sequences.into_iter().next().unwrap(), non_terminals)),
            _ => Ok((Expression::Choice(sequences), non_terminals)),
        }
    }

    fn process_sequence(&self, token: &Token) -> Result<(Expression, HashSet<String>)> {
        // expected shape [(Prefix, Primary, Suffix)*]

        let tokens = Self::tokens_for_non_terminal(SEQUENCE_STR, token)?;
        if tokens.len() % 3 != 0 {
            return Err(WrongNumberOfTokens(format!(
                "Expected a multiple of 3 many tokens, but got {}",
                tokens.len()
            )));
        }

        let mut parts = vec![];
        let mut non_terminals = HashSet::new();

        for [prefix, primary, suffix] in tokens.iter().chunk_fixed() {
            let (expression, seen_non_terminals) = self.process_primary(primary)?;
            let expression = self.process_suffix(suffix, expression)?; // suffix has higher precedence
            let expression = self.process_prefix(prefix, expression)?;

            parts.push(expression);
            for non_terminal in seen_non_terminals {
                non_terminals.insert(non_terminal);
            }
        }

        Ok((
            match parts.len() {
                0 => Expression::Empty,
                1 => parts.into_iter().next().unwrap(),
                _ => Expression::Sequence(parts),
            },
            non_terminals,
        ))
    }

    fn process_prefix(&self, prefix: &Token, expression: Expression) -> Result<Expression> {
        // expected shape [AND / OR]?
        let tokens = Self::tokens_for_non_terminal(PREFIX_STR, prefix)?;
        match tokens.len() {
            0 => Ok(expression),
            1 => {
                let token = &tokens[0];
                if Self::is_non_terminal(AND_STR, token) {
                    Ok(Expression::And(expression.into_rc_box()))
                } else if Self::is_non_terminal(NOT_STR, token) {
                    Ok(Expression::Not(expression.into_rc_box()))
                } else {
                    Err(UnExpectedToken(format!(
                        "Expected one of '{AND_STR}', '{NOT_STR}'"
                    )))
                }
            }
            l => Err(WrongNumberOfTokens(format!(
                "Expected 0 or 1 but got {}",
                l
            ))),
        }
    }

    fn process_suffix(&self, suffix: &Token, expression: Expression) -> Result<Expression> {
        // expected shape [QUESTION / STAR / PLUS]?
        let tokens = Self::tokens_for_non_terminal(SUFFIX_STR, suffix)?;
        match tokens.len() {
            0 => Ok(expression),
            1 => {
                let token = &tokens[0];
                if Self::is_non_terminal(QUESTION_STR, token) {
                    Ok(Expression::Optional(expression.into_rc_box()))
                } else if Self::is_non_terminal(STAR_STR, token) {
                    Ok(Expression::ZeroOrMore(expression.into_rc_box()))
                } else if Self::is_non_terminal(PLUS_STR, token) {
                    Ok(Expression::OneOrMore(expression.into_rc_box()))
                } else {
                    Err(UnExpectedToken(format!(
                        "Expected one of '{QUESTION_STR}', '{STAR_STR}', '{PLUS_STR}'"
                    )))
                }
            }
            l => Err(WrongNumberOfTokens(format!(
                "Expected 0 or 1 but got {}",
                l
            ))),
        }
    }

    fn process_primary(&self, primary: &Token) -> Result<(Expression, HashSet<String>)> {
        // expected shape ((Identifier !LEFTARROW) / (OPEN Expression CLOSE) / Literal / Class / DOT)

        let tokens = Self::tokens_for_non_terminal(PRIMARY_STR, primary)?;
        match tokens.as_slice() {
            [open, expression, close] => {
                Self::expect_non_terminal(OPEN_STR, open)?;
                Self::expect_non_terminal(CLOSE_STR, close)?;
                Ok(self.process_expression(expression)?)
            }
            [terminal] => {
                let name = Self::name_for_non_terminal(terminal)?;
                if name == IDENTIFIER_STR {
                    let non_terminal = self.process_identifier(terminal)?;
                    Ok((
                        Expression::NonTerminal(non_terminal.clone()),
                        HashSet::from([non_terminal]),
                    ))
                } else if name == LITERAL_STR {
                    Ok((self.process_literal(terminal)?, Default::default()))
                } else if name == CLASS_STR {
                    Ok((self.process_class(terminal)?, Default::default()))
                } else if name == DOT_STR {
                    Ok((Expression::Any, Default::default()))
                } else {
                    Err(UnExpectedToken(format!(
                        "Expected {IDENTIFIER_STR}, {LITERAL_STR}, {CLASS_STR} or {DOT_STR} but got {}",
                        name
                    )))
                }
            }
            _ => Err(WrongNumberOfTokens(format!(
                "Expected either 1, 2 or 3 tokens but got '{}'",
                tokens.len()
            ))),
        }
    }

    fn process_literal(&self, token: &Token) -> Result<Expression> {
        // expected shape
        // (('\'' (!'\'' Char)* '\'' Spacing)
        //      / ('\"' (!'\"' Char)* '\"' Spacing))
        // [q, (not quote, char)*, quote, spacing]

        let tokens = Self::tokens_for_non_terminal(LITERAL_STR, token)?;
        match tokens.as_slice() {
            [_, chars @ .., _, _] => {
                let mut literal = String::new();
                for [char] in chars.iter().chunk_fixed() {
                    literal.push_str(&self.process_char(char)?);
                }
                Ok(Literal(literal))
            }
            _ => Err(WrongNumberOfTokens(format!(
                "Expected 3 + x tokens, but got {}",
                tokens.len()
            ))),
        }
    }

    fn process_char(&self, token: &Token) -> Result<String> {
        // Expected shape
        // (('\\' ['t"\[nr\]\\])
        //      / ('\\' [0-9]+)
        //      / (!'\\' .))

        Self::expect_non_terminal(CHAR_STR, token)?;
        let Token::NonTerminal(_, Capture(from, to), _) = token else {
            panic!("Can't happen!")
        };
        let char = self.input_string[*from..*to]
            .to_string()
            .replace("\\'", "'")
            .replace("\\t", "\t")
            .replace("\\\"", "\"")
            .replace("\\[", "[")
            .replace("\\n", "\n")
            .replace("\\r", "\r")
            .replace("\\]", "]")
            .replace("\\\\", "\\");

        Ok(char)
    }

    fn process_class(&self, token: &Token) -> Result<Expression> {
        // expected shape
        // ('[' (!']' ClassMember)* ']' Spacing)
        // [, (not ], ClassMember*)
        let tokens = Self::tokens_for_non_terminal(CLASS_STR, token)?;

        match tokens.as_slice() {
            [_, members @ .., _, _] => {
                let mut parts = vec![];
                let mut symbols = HashSet::new();

                for member in members {
                    let members = Self::tokens_for_non_terminal(CLASS_MEMBER_STR, member)?;

                    for [member] in members.iter().chunk_fixed() {
                        if Self::is_non_terminal(CHAR_STR, member) {
                            symbols.insert(self.process_char(member)?);
                        } else {
                            parts.push(self.process_range(member)?);
                        }
                    }
                }

                if !symbols.is_empty() {
                    parts.push(Expression::Class(symbols))
                }

                Ok(match parts.len() {
                    0 => Expression::Empty,
                    1 => parts.into_iter().next().unwrap(),
                    _ => Expression::Choice(parts),
                })
            }
            _ => Err(WrongNumberOfTokens(format!(
                "Expected 3 + x tokens, but got {}",
                tokens.len()
            ))),
        }
    }

    fn process_range(&self, token: &Token) -> Result<Expression> {
        // (Char '-' Char)
        let tokens = Self::tokens_for_non_terminal(RANGE_STR, token)?;
        if let [from, _, to] = tokens.as_slice() {
            let from = self.process_char(from)?;
            let to = self.process_char(to)?;
            Ok(Expression::Range(from, to))
        } else {
            Err(WrongNumberOfTokens(format!(
                "Expected 3 tokens, got {}",
                tokens.len()
            )))
        }
    }
}

trait DestructureableChunks<'a, T: 'a> {
    fn chunk_fixed<const N: usize>(&self) -> impl Iterator<Item = &'a [T; N]>;
}

impl<'a, T: 'a> DestructureableChunks<'a, T> for Iter<'a, T> {
    fn chunk_fixed<const N: usize>(&self) -> impl Iterator<Item = &'a [T; N]> {
        self.as_slice()
            .chunks_exact(N)
            .map(|chunk| <&[T; N]>::try_from(chunk).unwrap())
    }
}
