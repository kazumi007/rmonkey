use std::collections::HashMap;

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

#[derive(PartialEq, Debug, Clone)]
pub enum PreOp {
    Neg, // -
    Not, // !
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
}

pub type ModifierFn = FnMut(Expression) -> Expression;

pub fn modify(program: Program, modifier: &mut FnMut(Expression) -> Expression) -> Program {
    let new_statement = program
        .statements
        .into_iter()
        .map(|s| modify_statement(s, modifier))
        .collect();
    Program {
        statements: new_statement,
    }
}

pub fn modify_statement(
    statement: Statement,
    modifier: &mut FnMut(Expression) -> Expression,
) -> Statement {
    match statement {
        Statement::Let(ident, expr) => Statement::Let(ident, modify_expression(expr, modifier)),
        Statement::Return(expr) => Statement::Return(modify_expression(expr, modifier)),
        Statement::Expression(expr) => Statement::Expression(modify_expression(expr, modifier)),
        Statement::BlockStatement(stmts) => {
            let new_stmts = stmts
                .into_iter()
                .map(|stmt| modify_statement(stmt, modifier))
                .collect();
            Statement::BlockStatement(new_stmts)
        }
        _ => statement,
    }
}

pub fn modify_expression(
    expr: Expression,
    modifier: &mut FnMut(Expression) -> Expression,
) -> Expression {
    match expr {
        Expression::Prefix(op, right) => {
            let mod_right = modifier(*right);
            Expression::Prefix(op, Box::new(mod_right))
        }
        Expression::Infix(left, op, right) => {
            let mod_left = modifier(*left);
            let mod_right = modifier(*right);
            Expression::Infix(Box::new(mod_left), op, Box::new(mod_right))
        }
        Expression::ArrayLiteral(vals) => {
            let mod_vals = vals.into_iter().map(|val| modifier(val)).collect();
            Expression::ArrayLiteral(mod_vals)
        }
        Expression::HashLiteral(vals) => {
            let mod_vals = vals
                .into_iter()
                .map(|val| (modifier(val.0), modifier(val.1)))
                .collect();
            Expression::HashLiteral(mod_vals)
        }
        Expression::FunctionLiteral(params, block) => {
            let mod_params = params
                .into_iter()
                .map(|param| modify_expression(param, modifier))
                .collect();
            let mod_blocks = modify_statement(*block, modifier);
            Expression::FunctionLiteral(mod_params, Box::new(mod_blocks))
        }
        Expression::Index(left, right) => {
            let mod_left = modifier(*left);
            let mod_right = modifier(*right);
            Expression::Index(Box::new(mod_left), Box::new(mod_right))
        }
        Expression::If(cond, conseq, alt) => {
            let mod_cond = modifier(*cond);
            let mod_conseq = modify_statement(*conseq, modifier);
            let mod_alt = match alt {
                Some(alt_v) => Some(Box::new(modify_statement(*alt_v, modifier))),
                _ => None,
            };
            Expression::If(Box::new(mod_cond), Box::new(mod_conseq), mod_alt)
        }

        _ => modifier(expr),
    }
}

// fn modify_expression<F>(expr: Expression, modifier: F) -> Expression
// where
//     F: Fn(Expression) -> Expression,
// {
//     modifier(expr)
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_modify_expr() {
        let mut turn_one_into_two = |expr: Expression| -> Expression {
            match expr {
                Expression::IntegerLiteral(1) => Expression::IntegerLiteral(2),
                _ => expr,
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
            assert_eq!(*expected, modified);
        }
    }

    #[test]
    fn test_modify_stmt() {
        let mut turn_one_into_two = |expr: Expression| -> Expression {
            match expr {
                Expression::IntegerLiteral(1) => Expression::IntegerLiteral(2),
                _ => expr,
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
            assert_eq!(*expected, modified);
        }
    }

    #[test]
    fn test_modify() {
        let mut turn_one_into_two = |expr: Expression| -> Expression {
            match expr {
                Expression::IntegerLiteral(1) => Expression::IntegerLiteral(2),
                _ => expr,
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
            assert_eq!(*expected, modified);
        }
    }
}
