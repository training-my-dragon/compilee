use std::fmt;

#[derive(Debug)]
pub struct Program {
    pub statement_list: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for statement in &self.statement_list {
            match write!(f, "{}", statement) {
                Ok(()) => (),
                Err(x) => return Err(x),
            }
        }

        Ok(())
    }
}

impl Program {
    pub fn new(statement_list: Vec<Statement>) -> Self {
        Program { statement_list: statement_list }
    }

    pub fn print_expression_tree(&self) {
        for statement in &self.statement_list {
            use self::Statement::*;
            match statement {
                Assign(lvalue, rvalue) => {

                },
                _ => (),
            };
        }
    }
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

impl Statement {
    pub fn generate_three_adress_code(&self) -> String {
        use self::Statement::*;

        match self {
            Assign(l, r) => {
                let (l_value_name, l_code) = l.generate_three_adress_code();
                let (r_value_name, r_code) = r.generate_three_adress_code();

                format!("{}{}{} = {}\n", l_code, r_code, l_value_name, r_value_name)
            }
            _ => String::new(),
        }
    }
}

impl LValue {
    pub fn generate_three_adress_code(&self) -> (String, String) {
        use self::LValue::*;

        match self {
            Id(name) => {
                (name.clone(), "".to_owned())
            },
            Access(lvalue, expr) => {
                let (l_value_name, l_code) = lvalue.generate_three_adress_code();
                let (e_value_name, e_code) = expr.generate_three_adress_code();

                let temp = "temp";
                (temp.to_owned(), format!("{}{}{} = {}[{}]\n",l_code, e_code, temp, l_value_name, e_value_name))
            },
        }
    }
}

impl RValue {
    pub fn generate_three_adress_code(&self) -> (String, String) {
        use self::RValue::*;

        match self {
            Expr(expr) => {
                expr.generate_three_adress_code()
            },
            _ => (String::new(), String::new()),
        }
    }
}

impl Expr {
    pub fn generate_three_adress_code(&self) -> (String, String) {
        use self::Expr::*;

        match self {
            BinaryOp(l, op, r) => {
                let (l_value_name, l_code) = l.generate_three_adress_code();
                let (r_value_name, r_code) = r.generate_three_adress_code();

                let temp = "temp";
                (temp.to_owned(), format!("{}{}{} = {} {} {}\n", l_code, r_code, temp, l_value_name, op, r_value_name))
            },
            UnaryOp(op, r) => {
                let (r_value_name, r_code) = r.generate_three_adress_code();

                let temp = "temp";
                (temp.to_owned(), format!("{}{} = {} {} {}\n", r_code, temp, -1, op, r_value_name))
            },
            NamedLeaf(lvalue) => {
                lvalue.generate_three_adress_code()
            },
            IntConstant(int) => (int.to_string(), String::new()),
            FloatConstant(float) => (float.to_string(), String::new()),
            _ => (String::new(), String::new()),
        }
    }
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
            Assign(_lvalue, _rvalue) => {
                let code = self::Statement::generate_three_adress_code(&self);

                write!(f, "{}", code)
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
            LessThan => write!(f, "<"),
            LessThanEq => write!(f, "<="),
            GreaterThan => write!(f, ">"),
            GreaterThanEq => write!(f, ">="),

            Equal => write!(f, "mul"),
            NotEqual => write!(f, "mul"),
            Mul => write!(f, "mul"),
            Div => write!(f, "div"),
            Add => write!(f, "add"),
            Sub => write!(f, "sub"),
            Mod => write!(f, "mod"),
            UnaryMinus => write!(f, "mul"),
        }
    }
}

