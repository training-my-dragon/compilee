pub mod ast;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub calculator);

#[derive(Debug, Clone)]
pub enum Token {
    Identifier,
}

#[test]
fn calculator_test() {
    calculator::ProgramParser::new().parse("{\
            int __banana55;\
            banana = 55;\
            banana = 77;
        }").unwrap();
}

#[test]
fn string_regex_test() {
    calculator::ProgramParser::new().parse("{\
            string __lalala55;\
            lalala = \"pud\"im\";
        }").unwrap();
}
