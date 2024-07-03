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

impl PEG {
    pub fn from_grammar(grammar: &str) -> Result<Self, PEGError> {
        // make actual error structure
        match Self::peg_parser().parse(grammar) {
            ParserOutput(_, _, Ok(tokens)) => match (Transformer {
                input_string: grammar,
            })
            .transform(tokens)
            {
                Ok(peg) => Ok(peg),
                Err(err) => Err(TransformationError(err)),
            },
            ParserOutput(_, _, Err(err)) => Err(ParsingError(err)),
        }
    }

    pub fn parse(&self, input: &str) -> ParserOutput {
        debug!("Begin Parsing");

        let result =
            Expression::NonTerminal(String::from("Grammar")).parse(&self.rules, input, 0, 0);

        debug!("Finished Parsing");

        result
    }

    /// a PEG that parses the PEG syntax itself
    pub fn peg_parser() -> Self {
        use crate::peg::expression::Expression::*;

        let start = String::from("Grammar");
        let mut rules: HashMap<_, _> = Default::default();

        macro_rules! add_rule {
            ($name:expr => $expression:expr) => {{
                rules.insert(String::from($name), $expression);
                NonTerminal(String::from($name))
            }};
        }

        let end_of_file = add_rule!("EndOfFile" => Not(Rc::new(Box::new(Any))));

        let end_of_line = add_rule!("EndOfLine" => Choice(vec![
            Literal(String::from("\r\n")),
            Literal(String::from("\r")),
            Literal(String::from("\n"))]));

        let space = add_rule!("Space" => Choice(vec![
            Literal(String::from(" ")),
            Literal(String::from("\t")),
            end_of_line.clone(),
        ]));

        let comment = add_rule!("Comment" => Sequence(vec![
            Literal(String::from("#")),
            ZeroOrMore(Sequence(vec![Not(end_of_line.clone().into_rc_box()), Any]).into_rc_box()),
            end_of_line.clone()
        ]));

        let spacing = add_rule!("Spacing" => ZeroOrMore(Choice(vec![
            space.clone(),
            comment.clone(),
        ]).into_rc_box()));

        let leftarrow =
            add_rule!("LEFTARROW" => Sequence(vec![Literal(String::from("<-")), spacing.clone()]));
        let slash =
            add_rule!("SLASH" => Sequence(vec![Literal(String::from("/")), spacing.clone()]));
        let and = add_rule!("AND" => Sequence(vec![Literal(String::from("&")), spacing.clone()]));
        let not = add_rule!("NOT" => Sequence(vec![Literal(String::from("!")), spacing.clone()]));
        let question =
            add_rule!("QUESTION" => Sequence(vec![Literal(String::from("?")), spacing.clone()]));
        let star = add_rule!("STAR" => Sequence(vec![Literal(String::from("*")), spacing.clone()]));
        let plus = add_rule!("PLUS" => Sequence(vec![Literal(String::from("+")), spacing.clone()]));
        let open = add_rule!("OPEN" => Sequence(vec![Literal(String::from("(")), spacing.clone()]));
        let close =
            add_rule!("CLOSE" => Sequence(vec![Literal(String::from(")")), spacing.clone()]));
        let dot = add_rule!("DOT" => Sequence(vec![Literal(String::from(".")), spacing.clone()]));
        let char = add_rule!("Char" => Choice(vec![
            Sequence(vec![Literal(String::from("\\")), Expression::make_class(&["n", "r", "t", "'", "\"", "[", "]", "\\"])]),
            Sequence(vec![
                Literal(String::from("\\")),
                OneOrMore(Range(String::from("0"), String::from("9")).into_rc_box()),
                Literal(String::from(";"))
            ]),
            Sequence(vec![Not(Literal(String::from("\\")).into_rc_box()), Any])
        ]));

        let range = add_rule!("Range" => Sequence(vec![
            char.clone(),
            Literal(String::from("-")),
            char.clone()
        ]));

        let class_member = add_rule!("ClassMember" => Choice(vec![
            range.clone(),
            char.clone()
        ]));

        let class = add_rule!("Class" => Sequence(vec![
            Literal(String::from("[")),
            ZeroOrMore(Sequence(vec![Not(Literal(String::from("]")).into_rc_box()), class_member.clone()]).into_rc_box()),
            Literal(String::from("]")),
            spacing.clone(),
        ]));

        let literal = add_rule!("Literal" => Choice(vec![
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

        let identifier = add_rule!("Identifier" => Sequence(vec![
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

        let expression = NonTerminal(String::from("Expression")); // cyclical

        let primary = add_rule!("Primary" => Choice(vec![
            Sequence(vec![identifier.clone(), Not(leftarrow.clone().into_rc_box())]),
            Sequence(vec![open.clone(), expression.clone(), close.clone()]),
            literal.clone(),
            class.clone(),
            dot.clone(),
        ]));

        let suffix = add_rule!("Suffix" => Optional(Choice(vec![
            question.clone(),
            star.clone(),
            plus.clone(),
        ]).into_rc_box()));

        let prefix = add_rule!("Prefix" => Optional(Choice(vec![
            and.clone(),
            not.clone(),
        ]).into_rc_box()));

        let sequence = add_rule!("Sequence" => ZeroOrMore(Sequence(vec![
            prefix.clone(),
            primary.clone(),
            suffix.clone(),
        ]).into_rc_box()));

        let expression = add_rule!("Expression" => Sequence(vec![
            sequence.clone(),
            ZeroOrMore(Sequence(vec![
                slash.clone(), sequence.clone()
            ]).into_rc_box())
        ]));

        let definition = add_rule!("Definition" => Sequence(vec![
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
