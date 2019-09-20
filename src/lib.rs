pub mod ast;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub calculator);

#[derive(Debug, Clone)]
pub enum Token {
    Identifier,
}

#[test]
fn calculator() {
    calculator::ProgramParser::new().parse("{ int __banana55; banana = 55; banana = 77; }").unwrap();
}
