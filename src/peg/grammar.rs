use crate::peg::expression::Expression;
use crate::peg::grammar::PEGError::{ParsingError, TransformationError};
use crate::peg::parsing::ParserOutput;
use crate::peg::transformer::{TransformError, Transformer};
use log::debug;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Clone, Debug)]
#[allow(dead_code)]
pub enum PEGError {
    ParsingError(()),
    TransformationError(TransformError),
}

#[derive(Clone, Debug)]
pub struct PEG {
    pub(crate) rules: HashMap<String, Expression>,
    pub(crate) start: String,
}

pub const GRAMMAR_STR: &'static str = "Grammar";
pub const END_OF_FILE_STR: &'static str = "EndOfFile";
pub const END_OF_LINE_STR: &'static str = "EndOfLine";
pub const SPACE_STR: &'static str = "Space";
pub const COMMENT_STR: &'static str = "Comment";
pub const SPACING_STR: &'static str = "Spacing";
pub const LEFT_ARROW_STR: &'static str = "LEFTARROW";
pub const SLASH_STR: &'static str = "SLASH";
pub const AND_STR: &'static str = "AND";
pub const NOT_STR: &'static str = "NOT";
pub const QUESTION_STR: &'static str = "QUESTION";
pub const STAR_STR: &'static str = "STAR";
pub const PLUS_STR: &'static str = "PLUS";
pub const OPEN_STR: &'static str = "OPEN";
pub const CLOSE_STR: &'static str = "CLOSE";
pub const DOT_STR: &'static str = "DOT";
pub const CHAR_STR: &'static str = "Char";
pub const RANGE_STR: &'static str = "Range";
pub const CLASS_MEMBER_STR: &'static str = "ClassMember";
pub const CLASS_STR: &'static str = "Class";
pub const LITERAL_STR: &'static str = "Literal";
pub const IDENTIFIER_STR: &'static str = "Identifier";
pub const EXPRESSION_STR: &'static str = "Expression";
pub const PRIMARY_STR: &'static str = "Primary";
pub const SUFFIX_STR: &'static str = "Suffix";
pub const PREFIX_STR: &'static str = "Prefix";
pub const SEQUENCE_STR: &'static str = "Sequence";
pub const DEFINITION_STR: &'static str = "Definition";

impl PEG {
    pub fn from_grammar(start: &str, grammar: &str) -> Result<Self, PEGError> {
        // make actual error structure
        match Self::peg_parser().parse(grammar) {
            ParserOutput(_, _, Ok(tokens)) => match (Transformer {
                input_string: grammar,
            })
            .transform(start, tokens)
            {
                Ok(peg) => Ok(peg),
                Err(err) => Err(TransformationError(err)),
            },
            ParserOutput(_, _, Err(err)) => Err(ParsingError(err)),
        }
    }

    pub fn parse(&self, input: &str) -> ParserOutput {
        debug!("Begin Parsing");

        let result = Expression::NonTerminal(self.start.clone()).parse(self, input, 0, 0);

        debug!("Finished Parsing");

        result
    }

    /// a PEG that parses the PEG syntax itself
    pub fn peg_parser() -> Self {
        use crate::peg::expression::Expression::*;

        let start = String::from(GRAMMAR_STR);
        let mut rules: HashMap<_, _> = Default::default();

        macro_rules! add_rule {
            ($name:expr => $expression:expr) => {{
                rules.insert(String::from($name), $expression);
                NonTerminal(String::from($name))
            }};
        }

        let end_of_file = add_rule!(END_OF_FILE_STR => Not(Rc::new(Box::new(Any))));

        let end_of_line = add_rule!(END_OF_LINE_STR => Choice(vec![
            Literal(String::from("\r\n")),
            Literal(String::from("\r")),
            Literal(String::from("\n"))]));

        let space = add_rule!(SPACE_STR => Choice(vec![
            Literal(String::from(" ")),
            Literal(String::from("\t")),
            end_of_line.clone(),
        ]));

        let comment = add_rule!(COMMENT_STR => Sequence(vec![
            Literal(String::from("#")),
            ZeroOrMore(Sequence(vec![Not(end_of_line.clone().into_rc_box()), Any]).into_rc_box()),
            end_of_line.clone()
        ]));

        let spacing = add_rule!(SPACING_STR => ZeroOrMore(Choice(vec![
            space.clone(),
            comment.clone(),
        ]).into_rc_box()));

        let leftarrow = add_rule!(LEFT_ARROW_STR => Sequence(vec![Literal(String::from("<-")), spacing.clone()]));
        let slash =
            add_rule!(SLASH_STR => Sequence(vec![Literal(String::from("/")), spacing.clone()]));
        let and = add_rule!(AND_STR => Sequence(vec![Literal(String::from("&")), spacing.clone()]));
        let not = add_rule!(NOT_STR => Sequence(vec![Literal(String::from("!")), spacing.clone()]));
        let question =
            add_rule!(QUESTION_STR => Sequence(vec![Literal(String::from("?")), spacing.clone()]));
        let star =
            add_rule!(STAR_STR => Sequence(vec![Literal(String::from("*")), spacing.clone()]));
        let plus =
            add_rule!(PLUS_STR => Sequence(vec![Literal(String::from("+")), spacing.clone()]));
        let open =
            add_rule!(OPEN_STR => Sequence(vec![Literal(String::from("(")), spacing.clone()]));
        let close =
            add_rule!(CLOSE_STR => Sequence(vec![Literal(String::from(")")), spacing.clone()]));
        let dot = add_rule!(DOT_STR => Sequence(vec![Literal(String::from(".")), spacing.clone()]));
        let char = add_rule!(CHAR_STR => Choice(vec![
            Sequence(vec![Literal(String::from("\\")), Expression::make_class(&["n", "r", "t", "'", "\"", "[", "]", "\\"])]),
            Sequence(vec![
                Literal(String::from("\\")),
                OneOrMore(Range(String::from("0"), String::from("9")).into_rc_box()),
                Literal(String::from(";"))
            ]),
            Sequence(vec![Not(Literal(String::from("\\")).into_rc_box()), Any])
        ]));

        let range = add_rule!(RANGE_STR => Sequence(vec![
            char.clone(),
            Literal(String::from("-")),
            char.clone()
        ]));

        let class_member = add_rule!(CLASS_MEMBER_STR => Choice(vec![
            range.clone(),
            char.clone()
        ]));

        let class = add_rule!(CLASS_STR => Sequence(vec![
            Literal(String::from("[")),
            ZeroOrMore(Sequence(vec![Not(Literal(String::from("]")).into_rc_box()), class_member.clone()]).into_rc_box()),
            Literal(String::from("]")),
            spacing.clone(),
        ]));

        let literal = add_rule!(LITERAL_STR => Choice(vec![
            Sequence(vec![
                Literal(String::from("'")),
                ZeroOrMore(Sequence(vec![
                    Not(Literal(String::from("'")).into_rc_box()),
                    char.clone(),
                ]).into_rc_box()),
                Literal(String::from("'")),
                spacing.clone(),
            ]),
            Sequence(vec![
                Literal(String::from("\"")),
                ZeroOrMore(Sequence(vec![
                    Not(Literal(String::from("\"")).into_rc_box()),
                    char.clone(),
                ]).into_rc_box()),
                Literal(String::from("\"")),
                spacing.clone(),
            ]),
        ]));

        let identifier = add_rule!(IDENTIFIER_STR => Sequence(vec![
            Choice(vec![
                Range(String::from("a"), String::from("z")),
                Range(String::from("A"), String::from("Z")),
                Literal(String::from("_")),
            ]),
            ZeroOrMore(Choice(vec![
                Range(String::from("a"), String::from("z")),
                Range(String::from("A"), String::from("Z")),
                Literal(String::from("_")),
                Range(String::from("0"), String::from("9")),
            ]).into_rc_box()),
            spacing.clone(),
        ]));

        let expression = NonTerminal(String::from(EXPRESSION_STR)); // cyclical

        let primary = add_rule!(PRIMARY_STR => Choice(vec![
            Sequence(vec![identifier.clone(), Not(leftarrow.clone().into_rc_box())]),
            Sequence(vec![open.clone(), expression.clone(), close.clone()]),
            literal.clone(),
            class.clone(),
            dot.clone(),
        ]));

        let suffix = add_rule!(SUFFIX_STR => Optional(Choice(vec![
            question.clone(),
            star.clone(),
            plus.clone(),
        ]).into_rc_box()));

        let prefix = add_rule!(PREFIX_STR => Optional(Choice(vec![
            and.clone(),
            not.clone(),
        ]).into_rc_box()));

        let sequence = add_rule!(SEQUENCE_STR => ZeroOrMore(Sequence(vec![
            prefix.clone(),
            primary.clone(),
            suffix.clone(),
        ]).into_rc_box()));

        let expression = add_rule!(EXPRESSION_STR => Sequence(vec![
            sequence.clone(),
            ZeroOrMore(Sequence(vec![
                slash.clone(), sequence.clone()
            ]).into_rc_box())
        ]));

        let definition = add_rule!(DEFINITION_STR => Sequence(vec![
            identifier.clone(),
            leftarrow.clone(),
            expression.clone(),
        ]));

        let _ = add_rule!(start.clone() => Sequence(vec![
            spacing.clone(),
            ZeroOrMore(definition.clone().into_rc_box()),
            end_of_file.clone(),
        ]));

        Self { start, rules }
    }
}

impl Display for PEG {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut rules = self
            .rules
            .iter()
            .map(|(name, expr)| format!("\t{} <- {}", name, expr))
            .collect::<Vec<_>>();
        rules.sort();
        write!(f, "PEG ({}) {{\n{}\n}}", self.start, rules.join("\n"))
    }
}
