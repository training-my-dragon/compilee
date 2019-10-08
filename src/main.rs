use std::env;
use std::fs::File;
use std::io::prelude::*;

use compilee::parser;

fn main() {
    let mut args: Vec<String> = env::args().collect();

    args.remove(0);

    println!("Compiling files: {:?}", args);

    for arg in args {

        let mut file = File::open(arg).expect("failed to open file.");

        let mut file_string = String::new();

        file.read_to_string(&mut file_string).unwrap();

        parser::ProgramParser::new().parse(&file_string).unwrap();
    }
}
