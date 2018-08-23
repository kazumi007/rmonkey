use std::fmt;

#[derive(PartialEq, Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    Let(Expression, Expression), // Expression::Identifier, Expression
    Return(Expression),
    Expression(Expression),
    BlockStatement(Vec<Statement>),
}

impl Statement {
    pub fn is_macro(&self) -> bool {
        match self {
            Statement::Let(_, value) => match value {
                Expression::MacroLiteral(_, _) => true,
                _ => false,
            },
            _ => false,
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (self) {
            Statement::Let(ident, expr) => write!(f, "let {} = {};", ident, expr),
            Statement::Return(expr) => write!(f, "return {};", expr),
            Statement::Expression(expr) => write!(f, "{}", expr),
            Statement::BlockStatement(stmts) => {
                for stmt in stmts {
                    write!(f, "{}", stmt);
                }
                Ok(())
            }
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum PreOp {
    Neg, // -
    Not, // !
}

impl fmt::Display for PreOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (self) {
            PreOp::Neg => write!(f, "-"),
            PreOp::Not => write!(f, "!"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum InfixOp {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Eq,  // ==
    Neq, // !=
    Lt,  // <
    Gt,  // >
}

impl fmt::Display for InfixOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (self) {
            InfixOp::Add => write!(f, "+"),
            InfixOp::Sub => write!(f, "-"),
            InfixOp::Mul => write!(f, "*"),
            InfixOp::Div => write!(f, "/"),
            InfixOp::Eq => write!(f, "=="),
            InfixOp::Neq => write!(f, "!="),
            InfixOp::Lt => write!(f, "<"),
            InfixOp::Gt => write!(f, ">"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Identifier(String),
    IntegerLiteral(i64),
    BooleanLiteral(bool),
    StringLiteral(String),
    Prefix(PreOp, Box<Expression>),
    ArrayLiteral(Vec<Expression>),
    Infix(Box<Expression>, InfixOp, Box<Expression>),
    HashLiteral(Vec<(Expression, Expression)>),
    FunctionLiteral(Vec<Expression>, Box<Statement>), //  Expression::Identifier, Expression::BlockStatement
    Index(Box<Expression>, Box<Expression>),          // Left[Index]
    If(Box<Expression>, Box<Statement>, Option<Box<Statement>>),
    Call(Box<Expression>, Vec<Expression>), // Function, Arguments
    MacroLiteral(Vec<Expression>, Box<Statement>), // Expression::Identifier, Expression::BlockStatement
}

fn join(exprs: &[Expression], separator: &str) -> String {
    let mut s = String::new();
    for (i, expr) in exprs.iter().enumerate() {
        s += &expr.to_string();
        if i != 0 {
            s += separator;
        }
    }
    s
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(val) => write!(f, "{}", val),
            Expression::IntegerLiteral(val) => write!(f, "{}", val),
            Expression::BooleanLiteral(val) => write!(f, "{}", val),
            Expression::StringLiteral(val) => write!(f, "{}", val),
            Expression::Prefix(op, expr) => write!(f, "({}{})", op, *expr),
            Expression::ArrayLiteral(exprs) => write!(f, "[{}]", join(exprs, ", ")),
            Expression::Infix(left, op, right) => write!(f, "({} {} {})", *left, op, *right),
            Expression::Index(ident, index) => write!(f, "{}[{}]", ident, index),
            Expression::FunctionLiteral(args, bstmt) => {
                write!(f, "fn({}){{{}}}", join(args, ", "), **bstmt)
            }
            Expression::If(cond, conseq, alter) => {
                if let Some(v) = alter {
                    write!(f, "if {} {} else {}", *cond, conseq, &*v)
                } else {
                    write!(f, "if {} {}", *cond, conseq)
                }
            }
            Expression::Call(ident, params) => write!(f, "{}({})", ident, join(params, ", ")),
            _ => write!(f, "not supported"),
        }
    }
}

pub type ModifierFn = FnMut(Expression) -> Result<Expression, String>;

pub fn modify(
    program: Program,
    modifier: &mut FnMut(Expression) -> Result<Expression, String>,
) -> Result<Program, String> {
    let mut new_stmt = vec![];
    for stmt in program.statements.into_iter() {
        new_stmt.push(modify_statement(stmt, modifier)?);
    }
    Ok(Program {
        statements: new_stmt,
    })
}

pub fn modify_statement(
    statement: Statement,
    modifier: &mut FnMut(Expression) -> Result<Expression, String>,
) -> Result<Statement, String> {
    match statement {
        Statement::Let(ident, expr) => {
            Ok(Statement::Let(ident, modify_expression(expr, modifier)?))
        }
        Statement::Return(expr) => Ok(Statement::Return(modify_expression(expr, modifier)?)),
        Statement::Expression(expr) => {
            Ok(Statement::Expression(modify_expression(expr, modifier)?))
        }
        Statement::BlockStatement(stmts) => {
            let mut new_stmts = vec![];
            for stmt in stmts.into_iter() {
                new_stmts.push(modify_statement(stmt, modifier)?);
            }
            Ok(Statement::BlockStatement(new_stmts))
        }
    }
}

pub fn modify_expression(
    expr: Expression,
    modifier: &mut FnMut(Expression) -> Result<Expression, String>,
) -> Result<Expression, String> {
    match expr {
        Expression::Prefix(op, right) => {
            let mod_right = modify_expression(*right, modifier)?;
            Ok(Expression::Prefix(op, Box::new(mod_right)))
        }
        Expression::Infix(left, op, right) => {
            let mod_left = modify_expression(*left, modifier)?;
            let mod_right = modify_expression(*right, modifier)?;
            Ok(Expression::Infix(
                Box::new(mod_left),
                op,
                Box::new(mod_right),
            ))
        }
        Expression::ArrayLiteral(vals) => {
            let mut new_vals = vec![];
            for val in vals {
                new_vals.push(modify_expression(val, modifier)?);
            }
            Ok(Expression::ArrayLiteral(new_vals))
        }
        Expression::HashLiteral(vals) => {
            let mut new_vals = vec![];
            for (key, val) in vals {
                let key = modify_expression(key, modifier)?;
                let val = modify_expression(val, modifier)?;
                new_vals.push((key, val));
            }
            Ok(Expression::HashLiteral(new_vals))
        }
        Expression::FunctionLiteral(params, block) => {
            let mut new_params = vec![];
            for param in params {
                new_params.push(modify_expression(param, modifier)?);
            }
            let mod_blocks = modify_statement(*block, modifier)?;
            Ok(Expression::FunctionLiteral(
                new_params,
                Box::new(mod_blocks),
            ))
        }
        Expression::Index(left, right) => {
            let mod_left = modify_expression(*left, modifier)?;
            let mod_right = modify_expression(*right, modifier)?;
            Ok(Expression::Index(Box::new(mod_left), Box::new(mod_right)))
        }
        Expression::If(cond, conseq, alt) => {
            let mod_cond = modify_expression(*cond, modifier)?;
            let mod_conseq = try!(modify_statement(*conseq, modifier));
            let mod_alt = match alt {
                Some(alt_v) => Some(Box::new(modify_statement(*alt_v, modifier)?)),
                _ => None,
            };
            Ok(Expression::If(
                Box::new(mod_cond),
                Box::new(mod_conseq),
                mod_alt,
            ))
        }

        _ => modifier(expr),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_modify_expr() {
        let mut turn_one_into_two = |expr: Expression| -> Result<Expression, String> {
            match expr {
                Expression::IntegerLiteral(1) => Ok(Expression::IntegerLiteral(2)),
                _ => Ok(expr),
            }
        };

        let one = || Expression::IntegerLiteral(1);
        let two = || Expression::IntegerLiteral(2);

        let tests = [
            (one(), two()),
            (
                Expression::Infix(Box::new(one()), InfixOp::Add, Box::new(two())),
                Expression::Infix(Box::new(two()), InfixOp::Add, Box::new(two())),
            ),
            (
                Expression::Infix(Box::new(two()), InfixOp::Add, Box::new(one())),
                Expression::Infix(Box::new(two()), InfixOp::Add, Box::new(two())),
            ),
            (
                Expression::Prefix(PreOp::Neg, Box::new(one())),
                Expression::Prefix(PreOp::Neg, Box::new(two())),
            ),
            (
                Expression::Index(Box::new(one()), Box::new(one())),
                Expression::Index(Box::new(two()), Box::new(two())),
            ),
            (
                Expression::If(
                    Box::new(one()),
                    Box::new(Statement::Expression(one())),
                    Some(Box::new(Statement::BlockStatement(vec![
                        Statement::Expression(one()),
                    ]))),
                ),
                Expression::If(
                    Box::new(two()),
                    Box::new(Statement::Expression(two())),
                    Some(Box::new(Statement::BlockStatement(vec![
                        Statement::Expression(two()),
                    ]))),
                ),
            ),
            (
                Expression::FunctionLiteral(
                    vec![Expression::Identifier("".to_string())],
                    Box::new(Statement::BlockStatement(vec![
                        Statement::Expression(one()),
                    ])),
                ),
                Expression::FunctionLiteral(
                    vec![Expression::Identifier("".to_string())],
                    Box::new(Statement::BlockStatement(vec![
                        Statement::Expression(two()),
                    ])),
                ),
            ),
            (
                Expression::ArrayLiteral(vec![one(), one()]),
                Expression::ArrayLiteral(vec![two(), two()]),
            ),
            (
                Expression::HashLiteral(vec![(one(), one())]),
                Expression::HashLiteral(vec![(two(), two())]),
            ),
        ];

        for (input, expected) in tests.iter() {
            let modified = modify_expression(input.clone(), &mut turn_one_into_two);
            match modified {
                Ok(val) => assert_eq!(*expected, val),
                Err(err) => panic!("err: {}", err),
            }
        }
    }

    #[test]
    fn test_modify_stmt() {
        let mut turn_one_into_two = |expr: Expression| -> Result<Expression, String> {
            match expr {
                Expression::IntegerLiteral(1) => Ok(Expression::IntegerLiteral(2)),
                _ => Ok(expr),
            }
        };

        let one = || Expression::IntegerLiteral(1);
        let two = || Expression::IntegerLiteral(2);

        let tests = [
            (Statement::Return(one()), Statement::Return(two())),
            (
                Statement::Let(Expression::Identifier("aaaa".to_string()), one()),
                Statement::Let(Expression::Identifier("aaaa".to_string()), two()),
            ),
        ];

        for (input, expected) in tests.into_iter() {
            let modified = modify_statement(input.clone(), &mut turn_one_into_two);
            match modified {
                Ok(val) => assert_eq!(*expected, val),
                Err(err) => panic!("err: {}", err),
            }
        }
    }

    #[test]
    fn test_modify() {
        let mut turn_one_into_two = |expr: Expression| -> Result<Expression, String> {
            match expr {
                Expression::IntegerLiteral(1) => Ok(Expression::IntegerLiteral(2)),
                _ => Ok(expr),
            }
        };

        let one = || Expression::IntegerLiteral(1);
        let two = || Expression::IntegerLiteral(2);

        let tests = [(
            Program {
                statements: vec![Statement::Expression(one())],
            },
            Program {
                statements: vec![Statement::Expression(two())],
            },
        )];

        for (input, expected) in tests.into_iter() {
            let modified = modify(input.clone(), &mut turn_one_into_two);
            match modified {
                Ok(val) => assert_eq!(*expected, val),
                Err(err) => panic!("err: {}", err),
            }
        }
    }
}
