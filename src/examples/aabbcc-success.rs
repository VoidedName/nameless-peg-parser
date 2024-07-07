use nameless_peg_parser::peg::grammar::PEG;

mod example;
use example::SIMPLE_LANGUAGE;

fn main() {
    use env_logger;
    env_logger::builder().init();

    let (start, grammar) = SIMPLE_LANGUAGE;
    let p = PEG::from_grammar(start, grammar).unwrap();
    println!("{:#?}", p.parse("aabbcc"));
    println!("{}", p);
}
