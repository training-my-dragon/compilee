pub mod ast;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub calculator); // synthesized by LALRPOP

#[test]
fn calculator() {
    let expr = calculator::ExprParser::new().parse("22 * 44 + 66").unwrap();
    assert_eq!(&format!("{:?}", expr), "((22 * 44) + 66)");
}

fn main() {
    println!("Hello, world!");
}
