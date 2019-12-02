pub mod ast;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub parser);

#[test]
fn calculator() {
    parser::ProgramParser::new().parse("{ int __banana55; banana = 55; banana = 77; }").unwrap();
}
