pub mod ast;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub calculator);

#[test]
fn calculator() {
    calculator::ProgramParser::new().parse("int banana[];").unwrap();
}

fn main() {
    println!("Hello, world!");
}
