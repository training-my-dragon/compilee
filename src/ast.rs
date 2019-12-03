use std::fmt;

#[derive(Debug)]
pub enum Type {
    Int,
    Float,
    String,

    Array(Box<Type>, isize),
}

#[derive(Debug)]
pub enum Statement {
    Declaration,
    Assign(LValue, RValue),
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

#[derive(Debug)]
pub enum RValue {
    Expr(Expr),
    Alloc,
}

#[derive(Debug)]
pub enum LValue {
    Id(String),
    Access(Box<LValue>, Box<Expr>),
}

#[derive(Debug)]
pub enum Expr {
    BinaryOp(Box<Expr>, OpCode, Box<Expr>),
    UnaryOp(OpCode, Box<Expr>),

    NamedLeaf(LValue),

    IntConstant(i64),
    FloatConstant(f64),
    StringConstant(String),
    NullConstant,
}

#[derive(Debug)]
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

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Statement::*;
        match self {
            Block(statement_list) => {
                for statement in statement_list {
                    match write!(f, "{}", statement) {
                        Ok(_) => (),
                        Err(_) => (),
                    }
                }
                Ok(())
            }
            Assign(lvalue, rvalue) => {
                match lvalue {
                    _ => write!(f, "LValue = "),
                }.unwrap();

                match rvalue {
                    self::RValue::Expr(expr) => writeln!(f, "{}", expr),
                    _ => writeln!(f, "RValue"),
                }
            }
            _ => writeln!(f, "statement"),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Expr::*;

        match self {
            BinaryOp(ref left, op , ref right) => {
                write!(f, "{} {} {}", left, op, right)
            },
            IntConstant(i) => write!(f, "{}", i),
            _ => write!(f, "expr"),
        }
    }
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::OpCode::*;

        match *self {
            Mul => write!(f, "mul"),
            Div => write!(f, "div"),
            Add => write!(f, "add"),
            Sub => write!(f, "sub"),
            Mod => write!(f, "mod"),
            _ => Ok(()),
        }
    }
}

