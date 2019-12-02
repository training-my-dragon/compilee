// use std::fmt::{Debug, Error, Formatter};

pub enum Type {
    Int,
    Float,
    String,

    Array(Box<Type>, isize),
}

pub enum Statement {
    Declaration,
    Assing(LValue, RValue),
    Print,
    Read,
    Return,
    If,
    For,
    Block(Vec<Statement>),
    Break,
    Empty,
    Error,
}

pub enum RValue {
    Expr(Expr),
    Alloc,
}

pub enum LValue {
    Id(String),
    Access(Box<LValue>, Box<Expr>),
}

pub enum Expr {
    BinaryOp(Box<Expr>, OpCode, Box<Expr>),
    UnaryOp(OpCode, Box<Expr>),

    NamedLeaf(LValue),

    IntConstant(i64),
    FloatConstant(f64),
    StringConstant(String),
    NullConstant,
}

pub enum OpCode {
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    Equal,
    NotEqual,
    Mul,
    Div,
    Add,
    Sub,
    Mod,
    UnaryMinus,
}

// impl Debug for Expr {
//     fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
//         use self::Expr::*;
//         match *self {
//             Number(n) => write!(fmt, "{:?}", n),
//             Op(ref l, op, ref r) => write!(fmt, "({:?} {:?} {:?})", l, op, r),
//             Error => write!(fmt, "error"),
//         }
//     }
// }
//
// impl Debug for Opcode {
//     fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
//         use self::Opcode::*;
//         match *self {
//             Mul => write!(fmt, "*"),
//             Div => write!(fmt, "/"),
//             Add => write!(fmt, "+"),
//             Sub => write!(fmt, "-"),
//         }
//     }
// }
