use std::collections::BTreeMap;
use std::fmt;

#[derive(Debug)]
pub struct Symbol(Type);

#[derive(Debug)]
pub struct SymbolTable {
    pub dictionary: BTreeMap<String, Symbol>,
    pub father: Option<Box<SymbolTable>>,
}

impl SymbolTable {
    pub fn new(dictionary: BTreeMap<String, Symbol>, father: Option<Box<SymbolTable>>) -> Self{
        SymbolTable { dictionary: dictionary, father: father }
    }
}

#[derive(Debug)]
pub struct Program {
    pub statement_list: Vec<Statement>,
    pub symbol_table: SymbolTable,
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
    pub fn new(statement_list: Vec<Statement>, symbol_table: SymbolTable ) -> Self {
        Program { statement_list: statement_list , symbol_table: symbol_table }
    }

    pub fn print_expression_tree(&self) {
        for statement in &self.statement_list {
            statement.print_expression_tree();
        }
    }

    pub fn populate_symbol_table(&mut self) {
        for statement in &mut self.statement_list {
            statement.populate_symbol_table(&mut self.symbol_table);
        }
    }

    pub fn print_symbol_table(&self) {
        println!("{:#?}", self.symbol_table);
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Float,
    String,
    Array(usize, Box<Type>),
}

#[derive(Debug)]
pub enum Statement {
    Declaration {
        id: String,
        decl_type: Type,
    },
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

impl Statement {
    pub fn print_expression_tree(&self) {
        use self::Statement::*;

        match self {
            Assign(lvalue, rvalue) => {
                lvalue.print_expression_tree();
                rvalue.print_expression_tree();
            }
            Block(statement_list) => {
                for statement in statement_list {
                    statement.print_expression_tree();
                }
            }
            _ => (),
        }
    }

    pub fn populate_symbol_table(&mut self, father: &mut SymbolTable) {
        use self::Statement::*;

        match self {
            Declaration{id, decl_type} => {
                father.dictionary.insert(id.clone(), Symbol(decl_type.clone()));
            },
            _ => (),
        }
    }
}

#[derive(Debug)]
pub enum RValue {
    Expr(Expr),
    Alloc,
}

impl RValue {
    pub fn print_expression_tree(&self) {
        use self::RValue::*;

        match self {
            Expr(e) => e.print_expression_tree(),
            _ => (),
        }
    }
}

#[derive(Debug)]
pub enum LValue {
    Id(String),
    Access(Box<LValue>, Box<Expr>),
}

impl LValue {
    pub fn print_expression_tree(&self) {
        use self::LValue::*;

        match self {
            Access(ref lvalue, ref expr) => {
                lvalue.print_expression_tree();
                expr.print_expression_tree();
            }
            _ => (),
        }
    }
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

impl Expr {
    pub fn print_expression_tree(&self) {
        println!("{:#?}", self);
    }
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

