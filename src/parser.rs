use ast::{Expression, InfixOp, PreOp, Program, Statement};
use lexer::Lexer;
use std::mem;
use token::Token;

macro_rules! matches {
    ($e:expr, $p:pat) => {
        match $e {
            $p => true,
            _ => false,
        }
    };
}

macro_rules! expect_peek {
    ($e:expr, $p:pat) => {
        if matches!($e.peek_token, $p) {
            $e.next_token();
        } else {
            return Err(format!(
                "expected token: {:?}, got={:?}",
                stringify!($p),
                $e.peek_token
            ).to_string());
        }
    };
}

#[derive(PartialOrd, PartialEq)]
enum Precedence {
    LOWEST,
    EQUALS,
    LESSGRATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
    INDEX,
}

pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: Token,
    peek_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(l: &'a mut Lexer) -> Parser<'a> {
        let mut parser = Parser {
            lexer: l,
            current_token: Token::EOF,
            peek_token: Token::EOF,
        };

        parser.next_token();
        parser.next_token();
        parser
    }

    pub fn parse_program(&mut self) -> Result<Program, String> {
        let mut program = Program { statements: vec![] };
        loop {
            if self.current_token == Token::EOF {
                break;
            }

            match self.parse_statement() {
                Ok(stmt) => program.statements.push(stmt),
                Err(err) => return Err(err),
            }
            self.next_token();
        }
        Ok(program)
    }

    fn next_token(&mut self) {
        mem::swap(&mut self.current_token, &mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    fn skip_semicolon(&mut self) {
        if matches!(self.peek_token, Token::SemiColon) {
            self.next_token();
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        expect_peek!(self, Token::Ident(_));

        let identifier = try!(self.parse_expression(Precedence::LOWEST));

        expect_peek!(self, Token::Assign);

        self.next_token();
        let value = try!(self.parse_expression(Precedence::LOWEST));

        self.skip_semicolon();

        Ok(Statement::Let(identifier, value))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        self.next_token();

        let value = try!(self.parse_expression(Precedence::LOWEST));

        self.skip_semicolon();

        Ok(Statement::Return(value))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let expression = try!(self.parse_expression(Precedence::LOWEST));

        self.skip_semicolon();

        Ok(Statement::Expression(expression))
    }

    fn parse_block_statement(&mut self) -> Result<Statement, String> {
        self.next_token();

        let mut vec = Vec::new();
        while !matches!(self.current_token, Token::RBrace)
            && !matches!(self.current_token, Token::EOF)
        {
            let stmt = try!(self.parse_statement());
            vec.push(stmt);
            self.next_token();
        }
        Ok(Statement::BlockStatement(vec))
    }

    fn parse_if_expression(&mut self) -> Result<Expression, String> {
        expect_peek!(self, Token::LParen);

        self.next_token();
        let cond = try!(self.parse_expression(Precedence::LOWEST));

        expect_peek!(self, Token::RParen);
        expect_peek!(self, Token::LBrace);

        let consequence = try!(self.parse_block_statement());

        let mut alternative = None;
        if matches!(self.peek_token, Token::Else) {
            self.next_token();

            expect_peek!(self, Token::LBrace);

            alternative = Some(Box::new(try!(self.parse_block_statement())));
        }

        Ok(Expression::If(
            Box::new(cond),
            Box::new(consequence),
            alternative,
        ))
    }

    fn parse_func(&mut self) -> Result<Expression, String> {
        expect_peek!(self, Token::LParen);

        let params = try!(self.parse_func_parameters());

        expect_peek!(self, Token::LBrace);

        let body = try!(self.parse_block_statement());

        Ok(Expression::FunctionLiteral(params, Box::new(body)))
    }

    fn parse_func_parameters(&mut self) -> Result<Vec<Expression>, String> {
        let mut params = Vec::new();
        if matches!(self.peek_token, Token::RParen) {
            self.next_token();
            return Ok(params);
        }

        self.next_token();
        params.push(try!(self.parse_identifier()));

        while matches!(self.peek_token, Token::Comma) {
            self.next_token();
            self.next_token();
            params.push(try!(self.parse_identifier()));
        }

        expect_peek!(self, Token::RParen);

        Ok(params)
    }

    fn parse_macro(&mut self) -> Result<Expression, String> {
        expect_peek!(self, Token::LParen);

        let params = try!(self.parse_func_parameters());

        expect_peek!(self, Token::LBrace);

        let block = try!(self.parse_block_statement());

        Ok(Expression::MacroLiteral(params, Box::new(block)))
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, String> {
        match self.current_token {
            Token::Ident(_) => self.parse_identifier(),
            Token::Int(_) => self.parse_int(),
            Token::False => self.parse_bool(),
            Token::True => self.parse_bool(),
            Token::Bang => self.parse_prefix(),
            Token::Minus => self.parse_prefix(),
            Token::LParen => self.parse_group(),
            Token::LBrace => self.parse_hash_literal(),
            Token::LBracket => self.parse_array_literal(),
            Token::Str(_) => self.parse_str(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_func(),
            Token::Macro => self.parse_macro(),
            _ => {
                return Err(format!(
                    "no prefix parse function for {:?} found.",
                    &self.current_token
                ).to_string())
            }
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, String> {
        let result = self.parse_prefix_expression();

        let mut left = try!(result);

        while !matches!(self.peek_token, Token::SemiColon) && (precedence < self.peek_precedence())
        {
            match self.peek_token {
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::Eq
                | Token::NotEq
                | Token::Lt
                | Token::Gt => {
                    self.next_token();
                    let result = self.parse_infix_expression(left);
                    left = try!(result);
                }
                Token::LParen => {
                    self.next_token();
                    let result = self.parse_call_expression(left);
                    left = try!(result);
                }
                Token::LBracket => {
                    self.next_token();
                    let result = self.parse_index_expression(left);
                    left = try!(result);
                }
                _ => return Ok(left),
            }
        }
       Ok(left)
    }

    fn parse_expression_list(&mut self) -> Result<Vec<Expression>, String> {
        let mut args = Vec::new();
        args.push(try!(self.parse_expression(Precedence::LOWEST)));

        while matches!(self.peek_token, Token::Comma) {
            self.next_token();
            self.next_token();
            args.push(try!(self.parse_expression(Precedence::LOWEST)));
        }

        Ok(args)
    }

    fn parse_call_expression(&mut self, func: Expression) -> Result<Expression, String> {
        let args;

        if matches!(self.peek_token, Token::RParen) {
            self.next_token();
            args = Vec::new();
        } else {
            self.next_token();
            args = try!(self.parse_expression_list());

            expect_peek!(self, Token::RParen);
        }

        Ok(Expression::Call(Box::new(func), args))
    }

    fn parse_index_expression(&mut self, left: Expression) -> Result<Expression, String> {
        self.next_token();
        let index = self.parse_expression(Precedence::LOWEST)?;

        expect_peek!(self, Token::RBracket);

        Ok(Expression::Index(Box::new(left), Box::new(index)))
    }

    fn precedence(token: &Token) -> Precedence {
        match token {
            Token::Eq | Token::NotEq => Precedence::EQUALS,
            Token::Gt | Token::Lt => Precedence::LESSGRATER,
            Token::Plus | Token::Minus => Precedence::SUM,
            Token::Asterisk | Token::Slash => Precedence::PRODUCT,
            Token::LParen => Precedence::CALL,
            Token::LBracket => Precedence::INDEX,
            _ => Precedence::LOWEST,
        }
    }

    fn current_precedence(&self) -> Precedence {
        Parser::precedence(&self.current_token)
    }

    fn peek_precedence(&self) -> Precedence {
        Parser::precedence(&self.peek_token)
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, String> {
        let op = match self.current_token {
            Token::Plus => InfixOp::Add,
            Token::Minus => InfixOp::Sub,
            Token::Asterisk => InfixOp::Mul,
            Token::Slash => InfixOp::Div,
            Token::Eq => InfixOp::Eq,
            Token::NotEq => InfixOp::Neq,
            Token::Lt => InfixOp::Lt,
            Token::Gt => InfixOp::Gt,
            _ => unreachable!(),
        };

        let precedence = self.current_precedence();

        self.next_token();
        let right = try!(self.parse_expression(precedence));

        Ok(Expression::Infix(Box::new(left), op, Box::new(right)))
    }

    fn parse_identifier(&mut self) -> Result<Expression, String> {
        match self.current_token {
            Token::Ident(ref x) => Ok(Expression::Identifier(x.clone())),
            _ => unreachable!(),
        }
    }

    fn parse_int(&mut self) -> Result<Expression, String> {
        match self.current_token {
            Token::Int(ref x) => x
                .parse::<i64>()
                .map_err(|err| err.to_string())
                .and_then(|val| Ok(Expression::IntegerLiteral(val))),
            _ => unreachable!(),
        }
    }

    fn parse_bool(&mut self) -> Result<Expression, String> {
        match self.current_token {
            Token::True => Ok(Expression::BooleanLiteral(true)),
            Token::False => Ok(Expression::BooleanLiteral(false)),
            _ => unreachable!(),
        }
    }

    fn parse_str(&mut self) -> Result<Expression, String> {
        match self.current_token {
            Token::Str(ref s) => Ok(Expression::StringLiteral(s.clone())),
            _ => unreachable!(),
        }
    }

    fn parse_array_literal(&mut self) -> Result<Expression, String> {
        let args;

        if matches!(self.peek_token, Token::RBracket) {
            self.next_token();
            args = Vec::new();
        } else {
            self.next_token();
            args = try!(self.parse_expression_list());

            expect_peek!(self, Token::RBracket);
        }

        Ok(Expression::ArrayLiteral(args))
    }

    fn parse_hash_literal(&mut self) -> Result<Expression, String> {
        let mut hash = vec![];

        while !matches!(self.peek_token, Token::RBrace) {
            self.next_token();
            let key = try!(self.parse_expression(Precedence::LOWEST));

            expect_peek!(self, Token::Colon);

            self.next_token();

            let value = try!(self.parse_expression(Precedence::LOWEST));
            hash.push((key, value));

            if !matches!(self.peek_token, Token::RBrace) {
                if !matches!(self.peek_token, Token::Comma) {
                    return Err(format!("invalid token: {:?}", self.peek_token).to_string());
                } else {
                    self.next_token();
                }
            }
        }

        expect_peek!(self, Token::RBrace);

        Ok(Expression::HashLiteral(hash))
    }

    fn parse_prefix(&mut self) -> Result<Expression, String> {
        let op = match self.current_token {
            Token::Bang => PreOp::Not,
            Token::Minus => PreOp::Neg,
            _ => unreachable!(),
        };

        self.next_token();

        let expr = try!(self.parse_expression(Precedence::PREFIX));

        Ok(Expression::Prefix(op, Box::new(expr)))
    }

    fn parse_group(&mut self) -> Result<Expression, String> {
        self.next_token();

        let exp = try!(self.parse_expression(Precedence::LOWEST));

        expect_peek!(self, Token::RParen);

        Ok(exp)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_statement() {
        // [input, identifier, value]
        let tests = [
            ("let x = 5;", "x", Expression::IntegerLiteral(5)),
            ("let y = true;", "y", Expression::BooleanLiteral(true)),
        ];

        for test in tests.iter() {
            let mut l = Lexer::with_string(test.0);
            let mut parser = Parser::new(&mut l);
            let program = parser.parse_program().unwrap();

            assert_eq!(1, program.statements.len());

            let stmt = &program.statements[0];
            assert!(matches!(stmt, Statement::Let(_, _)));
            match stmt {
                Statement::Let(ident, value) => {
                    assert!(test_let_identifier(ident, test.1));
                    assert_eq!(value, &test.2);
                }
                _ => panic!(),
            }
        }
    }

    fn test_let_identifier(expr: &Expression, name: &str) -> bool {
        match expr {
            Expression::Identifier(x) if x == name => true,
            _ => false,
        }
    }

    #[test]
    fn test_return_statement() {
        let inputs = r#"return 1;
return 10;
return 993322;
"#;

        let mut l = Lexer::with_string(inputs);
        let mut parser = Parser::new(&mut l);
        let program = parser.parse_program().unwrap();

        assert_eq!(3, program.statements.len());

        for t in program.statements.into_iter() {
            match t {
                Statement::Return(_) => (),
                _ => panic!(format!("unexpected ast type: {:?}", t)),
            }
        }
    }

    #[test]
    fn test_prefix_expression() {
        let tests = [
            ("!5", PreOp::Not, Expression::IntegerLiteral(5)),
            ("-10", PreOp::Neg, Expression::IntegerLiteral(10)),
            ("!true", PreOp::Not, Expression::BooleanLiteral(true)),
            ("!false", PreOp::Not, Expression::BooleanLiteral(false)),
        ];

        for t in tests.into_iter() {
            let mut l = Lexer::with_string(t.0);
            let mut parser = Parser::new(&mut l);
            let program = parser.parse_program().unwrap();

            assert_eq!(1, program.statements.len());
            for s in program.statements.into_iter() {
                match s {
                    Statement::Expression(e) => {
                        match e {
                            Expression::Prefix(op, value) => {
                                assert_eq!(t.1, op);
                                assert_eq!(t.2, *value);
                            }
                            _ => panic!(format!("unexpected ast type: {:?}", e)),
                        };
                    }
                    _ => panic!(format!("unexpected ast type: {:?}", s)),
                };
            }
        }
    }

    #[test]
    fn test_infix_expression() {
        let tests = [
            (
                "5 + 5",
                Expression::IntegerLiteral(5),
                InfixOp::Add,
                Expression::IntegerLiteral(5),
            ),
            (
                "5 - 5",
                Expression::IntegerLiteral(5),
                InfixOp::Sub,
                Expression::IntegerLiteral(5),
            ),
            (
                "5 * 5",
                Expression::IntegerLiteral(5),
                InfixOp::Mul,
                Expression::IntegerLiteral(5),
            ),
            (
                "5 / 5",
                Expression::IntegerLiteral(5),
                InfixOp::Div,
                Expression::IntegerLiteral(5),
            ),
            (
                "5 == 5",
                Expression::IntegerLiteral(5),
                InfixOp::Eq,
                Expression::IntegerLiteral(5),
            ),
            (
                "5 != 5",
                Expression::IntegerLiteral(5),
                InfixOp::Neq,
                Expression::IntegerLiteral(5),
            ),
            (
                "5 < 5",
                Expression::IntegerLiteral(5),
                InfixOp::Lt,
                Expression::IntegerLiteral(5),
            ),
            (
                "5 > 5",
                Expression::IntegerLiteral(5),
                InfixOp::Gt,
                Expression::IntegerLiteral(5),
            ),
            (
                "true == true",
                Expression::BooleanLiteral(true),
                InfixOp::Eq,
                Expression::BooleanLiteral(true),
            ),
            (
                "true != false",
                Expression::BooleanLiteral(true),
                InfixOp::Neq,
                Expression::BooleanLiteral(false),
            ),
            (
                "false != false",
                Expression::BooleanLiteral(false),
                InfixOp::Neq,
                Expression::BooleanLiteral(false),
            ),
        ];

        for t in tests.into_iter() {
            let mut l = Lexer::with_string(t.0);
            let mut parser = Parser::new(&mut l);
            let program = parser.parse_program().unwrap();

            println!("{:?}\n", program.statements);
            assert_eq!(1, program.statements.len());
            for s in program.statements.into_iter() {
                match s {
                    Statement::Expression(e) => {
                        match e {
                            Expression::Infix(lhs, op, rhs) => {
                                assert_eq!(t.1, *lhs);
                                assert_eq!(t.2, op);
                                assert_eq!(t.3, *rhs);
                            }
                            _ => panic!(format!("unexpected ast type: {:?}", e)),
                        };
                    }
                    _ => panic!(format!("unexpected ast type: {:?}", s)),
                };
            }
        }
    }

    #[test]
    fn test_operator_precedence() {
        let tests = [(
            "-a * b",
            "Expression(Infix(Prefix(Neg, Identifier(\"a\")), Mul, Identifier(\"b\")))",
        ),
        (
            "!-a",
            "Expression(Prefix(Not, Prefix(Neg, Identifier(\"a\"))))",
        ),
        (
            "a + b + c",
            "Expression(Infix(Infix(Identifier(\"a\"), Add, Identifier(\"b\")), Add, Identifier(\"c\")))",
        ),
        (
            "a + b - c",
            "Expression(Infix(Infix(Identifier(\"a\"), Add, Identifier(\"b\")), Sub, Identifier(\"c\")))",
        ),
        (
            "a * b / c",
            "Expression(Infix(Infix(Identifier(\"a\"), Mul, Identifier(\"b\")), Div, Identifier(\"c\")))",
        ),
        (
            "a + b * c + d / e - f",
            "Expression(Infix(Infix(Infix(Identifier(\"a\"), Add, Infix(Identifier(\"b\"), Mul, Identifier(\"c\"))), Add, Infix(Identifier(\"d\"), Div, Identifier(\"e\"))), Sub, Identifier(\"f\")))"
        ),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "Expression(Infix(Infix(IntegerLiteral(3), Add, Infix(IntegerLiteral(4), Mul, IntegerLiteral(5))), Eq, Infix(Infix(IntegerLiteral(3), Mul, IntegerLiteral(1)), Add, Infix(IntegerLiteral(4), Mul, IntegerLiteral(5)))))"
        ),
        (
            "true",
            "Expression(BooleanLiteral(true))",
        ),
        (
            "false",
            "Expression(BooleanLiteral(false))",
        ),
        (
			"3 > 5 == false",
			"Expression(Infix(Infix(IntegerLiteral(3), Gt, IntegerLiteral(5)), Eq, BooleanLiteral(false)))",
        ),
		(
			"3 < 5 == true",
			"Expression(Infix(Infix(IntegerLiteral(3), Lt, IntegerLiteral(5)), Eq, BooleanLiteral(true)))",
        ),
		(
            "1 + (2 + 3) + 4",
			"Expression(Infix(Infix(IntegerLiteral(1), Add, Infix(IntegerLiteral(2), Add, IntegerLiteral(3))), Add, IntegerLiteral(4)))",
        ),

        ];

        for t in tests.into_iter() {
            let mut l = Lexer::with_string(t.0);
            let mut parser = Parser::new(&mut l);
            let program = parser.parse_program().unwrap();

            assert_eq!(1, program.statements.len());
            assert_eq!(t.1, format!("{:?}", program.statements[0]));
        }
    }

    #[test]
    fn test_if_expression() {
        let input = r#"if (x < y) { x }"#;

        let mut l = Lexer::with_string(input);
        let mut parser = Parser::new(&mut l);
        let program = parser.parse_program().unwrap();

        assert_eq!(1, program.statements.len());

        match program.statements[0] {
            Statement::Expression(ref exp) => match exp {
                Expression::If(cond, cons, alt) => {
                    match **cond {
                        Expression::Infix(ref left, ref op, ref right) => {
                            assert_eq!(Expression::Identifier("x".to_string()), **left);
                            assert_eq!(InfixOp::Lt, *op);
                            assert_eq!(Expression::Identifier("y".to_string()), **right);
                        }
                        _ => panic!("invalid expression: {:?}", exp),
                    };
                    match **cons {
                        Statement::BlockStatement(ref stmts) => {
                            assert_eq!(1, stmts.len());
                            match &stmts[0] {
                                Statement::Expression(exp) => {
                                    assert_eq!(Expression::Identifier("x".to_string()), *exp);
                                }
                                _ => panic!("invalid statements {:?}", stmts[0]),
                            }
                        }
                        _ => panic!("invalid statements: {:?}", cons),
                    };

                    assert_eq!(true, alt.is_none());
                }
                _ => panic!("invalid expression: {:?}", exp),
            },
            _ => panic!("invalid statement: {:?}", program.statements[0]),
        }
    }

    #[test]
    fn test_function_literal() {
        let input = r#"fn(x, y){x + y;}"#;

        let mut l = Lexer::with_string(input);
        let mut p = Parser::new(&mut l);

        let program = p.parse_program().unwrap();

        assert_eq!(1, program.statements.len());
        match program.statements[0] {
            Statement::Expression(ref exp) => match exp {
                Expression::FunctionLiteral(args, bstmt) => {
                    assert_eq!(2, args.len());
                    assert_eq!(Expression::Identifier("x".to_string()), args[0]);
                    assert_eq!(Expression::Identifier("y".to_string()), args[1]);

                    match &**bstmt {
                        Statement::BlockStatement(stmts) => {
                            assert_eq!(1, stmts.len());
                            match &stmts[0] {
                                Statement::Expression(exp) => match exp {
                                    Expression::Infix(ref left, ref op, ref right) => {
                                        assert_eq!(Expression::Identifier("x".to_string()), **left);
                                        assert_eq!(InfixOp::Add, *op);
                                        assert_eq!(
                                            Expression::Identifier("y".to_string()),
                                            **right
                                        );
                                    }
                                    _ => panic!("unexpected ast: {:?}", exp),
                                },
                                _ => panic!("unexpected ast: {:?}", stmts[0]),
                            }
                        }
                        _ => panic!("unexpected ast: {:?}", bstmt),
                    }
                }
                _ => panic!("unexpected ast: {:?}", exp),
            },
            _ => panic!("unexpected ast: {:?}", program.statements[0]),
        }
    }

    #[test]
    fn test_call_expression() {
        let input = r#"add(1, 2 * 3, 4 + 5);"#;
        let mut l = Lexer::with_string(input);
        let mut p = Parser::new(&mut l);

        let program = p.parse_program().unwrap();
        assert_eq!(1, program.statements.len());

        match &program.statements[0] {
            Statement::Expression(exp) => match exp {
                Expression::Call(func, args) => {
                    assert_eq!(Expression::Identifier("add".to_string()), **func);
                    assert_eq!(3, args.len());
                }
                _ => panic!("unexpected ast:{:?}", exp),
            },
            _ => panic!("unexpected ast: {:?}", program.statements[0]),
        }
    }

    #[test]
    fn test_call_expression_2() {
        let input = "let add = fn(x, y){x + y}; puts(add(1, 2 * 3, 4 + 5));";
        let mut l = Lexer::with_string(input);
        let mut p = Parser::new(&mut l);

        let program = p.parse_program().unwrap();
        assert_eq!(2, program.statements.len());
        println!("program {:?}", program);
        // match &program.statements[0] {
        //     Statement::Expression(exp) => match exp {
        //         Expression::Call(func, args) => {
        //             assert_eq!(Expression::Identifier("add".to_string()), **func);
        //             assert_eq!(3, args.len());
        //         }
        //         _ => panic!("unexpected ast:{:?}", exp),
        //     },
        //     _ => panic!("unexpected ast: {:?}", program.statements[0]),
        // }
    }

    #[test]
    fn test_macro_literal() {
        let input = "macro(x, y){ x + y; }";

        let mut l = Lexer::with_string(input);
        let mut p = Parser::new(&mut l);

        let program = p.parse_program().unwrap();
        assert_eq!(1, program.statements.len());
        match &program.statements[0] {
            Statement::Expression(expr) => match expr {
                Expression::MacroLiteral(params, _) => {
                    assert_eq!(2, params.len());
                    test_let_identifier(&params[0], "x");
                    test_let_identifier(&params[1], "y");
                }
                _ => panic!("unexpected expr: {:?}", expr),
            },
            _ => panic!("unexpected stmt: {:?}", program.statements[0]),
        }
    }
}
