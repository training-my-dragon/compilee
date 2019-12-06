use std::collections::BTreeMap;
use std::fmt;

#[derive(Debug, Clone)]
pub struct Symbol(Type);

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub dictionary: BTreeMap<String, Symbol>,
    pub father: Option<Box<SymbolTable>>,
}

impl SymbolTable {
    pub fn new(dictionary: BTreeMap<String, Symbol>, father: Option<Box<SymbolTable>>) -> Self{
        SymbolTable { dictionary: dictionary, father: father }
    }

    pub fn get(&self, id: &String) -> Option<Symbol> {
        match self.dictionary.get(id) {
            Some(s) => Some(s.clone()),
            None => match &self.father {
                Some(table) => table.get(id),
                None => None,
            }
        }
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

        for statement in &self.statement_list {
            statement.print_symbol_table();
        }
    }

    pub fn run_type_checker(&self) -> Result<(), String> {
        for statement in &self.statement_list {
            match statement.run_type_checker(&self.symbol_table) {
                Ok(_) => (),
                Err(s) => return Err(s),
            };
        }
        Ok(())
    }

    pub fn check_break_inside_for(&self) -> Result<(), String> {
        for statement in &self.statement_list {
            match statement.check_break_inside_for(false) {
                Ok(_) => (),
                Err(s) => return Err(s),
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    String,
    Null,
    Array(usize, Box<Type>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Type::*;

        match *self {
            Int => write!(f, "int"),
            Float => write!(f, "float"),
            String => write!(f, "string"),
            _ => write!(f, "unknown"),
        }
    }
}


#[derive(Debug)]
pub enum Statement {
    Declaration {
        id: String,
        decl_type: Type,
    },
    FunctionDefinition,

    Assign(LValue, RValue),

    Print(Expr),
    Read(LValue),

    If {
        compare: Expr,
        true_block: Box<Statement>,
        false_block: Option<Box<Statement>>,
    },
    For {
        init_assign: Box<Statement>,
        compare: Expr,
        loop_assign: Box<Statement>,
        block: Box<Statement>,
    },
    Block(Vec<Statement>, SymbolTable),
    Return,
    Break,
    Empty,
}

impl Statement {
    pub fn print_expression_tree(&self) {
        use self::Statement::*;

        match self {
            Assign(lvalue, rvalue) => {
                lvalue.print_expression_tree();
                rvalue.print_expression_tree();
            }
            Block(statement_list, _) => {
                for statement in statement_list {
                    statement.print_expression_tree();
                }
            }
            Print(expr) => {
                expr.print_expression_tree();
            }
            Read(lvalue) => {
                lvalue.print_expression_tree();
            }
            If { compare, true_block, false_block } => {
                compare.print_expression_tree();
                true_block.print_expression_tree();
                match false_block {
                    Some(block) => block.print_expression_tree(),
                    None => (),
                }
            }
            For { init_assign, compare, loop_assign, block } => {
                init_assign.print_expression_tree();
                compare.print_expression_tree();
                loop_assign.print_expression_tree();
                block.print_expression_tree();
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
            Block(statement_list, symbol_table) => {
                symbol_table.father = Some(Box::new(father.clone()));

                for statement in statement_list {
                    statement.populate_symbol_table(symbol_table);
                }
            }
            If { compare: _, true_block, false_block } => {
                true_block.populate_symbol_table(father);

                match false_block {
                    Some(block) => block.populate_symbol_table(father),
                    None => (),
                }
            }
            For { init_assign: _, compare: _, loop_assign: _, block } => {
                block.populate_symbol_table(father);
            }
            _ => (),
        }
    }

    pub fn print_symbol_table(&self) {
        use self::Statement::*;

        match self {
            Block(statement_list, symbol_table) => {
                println!("{:#?}", symbol_table);

                for statement in statement_list {
                    statement.print_symbol_table();
                }
            }
            If { compare: _, true_block, false_block } => {
                true_block.print_symbol_table();

                match false_block {
                    Some(block) => block.print_symbol_table(),
                    None => (),
                }
            }
            For { init_assign: _, compare: _, loop_assign: _, block } => {
                block.print_symbol_table();
            }
            _ => (),
        }
    }

    pub fn run_type_checker(&self, symbol_table: &SymbolTable) -> Result<(), String> {
        use self::Statement::*;

        match self {
            Assign(lvalue, rvalue) => {
                match lvalue.run_type_checker(symbol_table) {
                    Ok(_) => (),
                    Err(s) => return Err(s),
                }
                match rvalue.run_type_checker(symbol_table) {
                    Ok(_) => (),
                    Err(s) => return Err(s),
                }
            }
            Block(statement_list, table) => {
                for statement in statement_list {
                    match statement.run_type_checker(table) {
                        Ok(_) => (),
                        Err(s) => return Err(s),
                    }
                }
            }
            Print(expr) => {
                match expr.run_type_checker(symbol_table) {
                    Ok(_) => (),
                    Err(s) => return Err(s),
                }
            }
            Read(lvalue) => {
                match lvalue.run_type_checker(symbol_table) {
                    Ok(_) => (),
                    Err(s) => return Err(s),
                }
            }
            If { compare, true_block, false_block } => {
                match compare.run_type_checker(symbol_table) {
                    Ok(_) => (),
                    Err(s) => return Err(s),
                }
                match true_block.run_type_checker(symbol_table) {
                    Ok(_) => (),
                    Err(s) => return Err(s),
                }
                match false_block {
                    Some(block) => match block.run_type_checker(symbol_table) {
                        Ok(_) => (),
                        Err(s) => return Err(s),
                    },
                    None => (),
                }
            }
            For { init_assign, compare, loop_assign, block } => {
                match init_assign.run_type_checker(symbol_table) {
                    Ok(_) => (),
                    Err(s) => return Err(s),
                }
                match compare.run_type_checker(symbol_table) {
                    Ok(_) => (),
                    Err(s) => return Err(s),
                }
                match loop_assign.run_type_checker(symbol_table) {
                    Ok(_) => (),
                    Err(s) => return Err(s),
                }
                match block.run_type_checker(symbol_table) {
                    Ok(_) => (),
                    Err(s) => return Err(s),
                }
            }
            _ => (),
        };

        Ok(())
    }

    pub fn check_break_inside_for(&self, can_break: bool) -> Result<(), String> {
        use self::Statement::*;

        match self {
            Block(statement_list, _) => {
                for statement in statement_list {
                    match statement.check_break_inside_for(can_break) {
                        Ok(_) => (),
                        Err(s) => return Err(s),
                    }
                }
            },
            For { init_assign, compare, loop_assign, block } => {
                return block.check_break_inside_for(true);
            }
            If { compare: _, true_block, false_block } => {
                match true_block.check_break_inside_for(can_break) {
                    Ok(_) => (),
                    Err(s) => return Err(s),
                }

                match false_block {
                    Some(block) => match block.check_break_inside_for(can_break) {
                        Ok(_) => (),
                        Err(s) => return Err(s),
                    }
                    None => (),
                }
            }
            Break => if can_break {
                return Ok(());
            } else {
                return Err(format!("break statement outside for loop"));
            }
            _ => (),
        };

        Ok(())
    }
}

#[derive(Debug)]
pub enum RValue {
    Expr(Expr),
    Alloc(Type),
}

impl RValue {
    pub fn print_expression_tree(&self) {
        use self::RValue::*;

        match self {
            Expr(e) => e.print_expression_tree(),
            _ => (),
        }
    }

    pub fn run_type_checker(&self, symbol_table: &SymbolTable) -> Result<Type, String> {
        use self::RValue::*;

        match self {
            Expr(e) => e.run_type_checker(symbol_table),
            Alloc(t) => Ok(t.clone()),
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

    pub fn run_type_checker(&self, symbol_table: &SymbolTable) -> Result<Type, String> {
        use self::LValue::*;

        match self {
            Access(ref lvalue, ref expr) => {
                match lvalue.run_type_checker(symbol_table) {
                    Ok(l_type) => {
                        match expr.run_type_checker(symbol_table) {
                            Ok(r_type) => {
                                if l_type == r_type {
                                    Ok(l_type)
                                } else {
                                    Err(format!("Type missmatch between {} and {}.", l_type, r_type))
                                }
                            }
                            Err(s) => Err(s),
                        }
                    }
                    Err(s) => Err(s),
                }
            }
            Id(id) => {
                match symbol_table.get(id) {
                    Some(symbol) => Ok(symbol.0.clone()),
                    None => Err(format!("Can not find {} on symble table", id)),
                }
            }
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

    pub fn run_type_checker(&self, symbol_table: &SymbolTable) -> Result<Type, String> {
        use self::Expr::*;

        match self {
            BinaryOp(l, _op, r) => {
                match l.run_type_checker(symbol_table) {
                    Ok(l_type) => {
                        match r.run_type_checker(symbol_table) {
                            Ok(r_type) => {
                                if l_type == r_type {
                                    Ok(l_type)
                                } else {
                                    Err(format!("Type missmatch between {} and {}", l_type, r_type))
                                }
                            }
                            Err(s) => Err(s),
                        }
                    }
                    Err(s) => Err(s),
                }
            },
            UnaryOp(_op, expr) => {
                expr.run_type_checker(symbol_table)
            },
            NamedLeaf(lvalue) => {
                lvalue.run_type_checker(symbol_table)
            },
            IntConstant(_) => Ok(self::Type::Int),
            FloatConstant(_) => Ok(self::Type::Float),
            StringConstant(_) => Ok(self::Type::String),
            NullConstant => Ok(self::Type::Null),
        }
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
            Block(statement_list, _) => {
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

