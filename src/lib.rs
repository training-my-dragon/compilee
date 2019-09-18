pub mod ast;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub calculator);

#[derive(Debug, Clone)]
pub enum Token {
    Identifier,
    Semicolon,
    Int,
    Float,
    String,

    LBracket,
    RBracket,
}

pub enum LexicalError {}

use std::str::CharIndices;

pub struct Lexer<'input> {
    chars: CharIndices<'input>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer {
            chars: input.char_indices(),
        }
    }
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.chars.next() {
                Some((i, ' ')) => return Some(Ok((i, Token::Semicolon, i + 1))),
                None => return None,
                _ => continue,
            }
        }
    }
}

#[test]
fn calculator() {
    let lexer = Lexer::new("int banana[];");
    calculator::ProgramParser::new().parse(lexer).ok();
}
