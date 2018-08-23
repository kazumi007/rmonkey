use ast::*;
use builtins::*;
use fnv::FnvHashMap;
use object::{Environment, Hashable, MObject};
use std::cell::RefCell;
use std::rc::Rc;

pub type EvalResult = Result<Rc<MObject>, String>;

pub type Env = Rc<RefCell<Environment>>;

pub struct Evaluator {
    builtin: FnvHashMap<String, BuiltinFunc>,
    true_obj: Rc<MObject>,
    false_obj: Rc<MObject>,
    null_obj: Rc<MObject>,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            builtin: builtinmap(),
            true_obj: Rc::new(MObject::Bool(true)),
            false_obj: Rc::new(MObject::Bool(false)),
            null_obj: Rc::new(MObject::Null),
        }
    }

    pub fn eval(&mut self, program: &Program, env: &Env) -> EvalResult {
        self.eval_statements(&program.statements, env)
    }

    fn eval_statements(&mut self, stmts: &[Statement], env: &Env) -> EvalResult {
        let mut value = self.null_obj.clone();
        for stmt in stmts {
            let val = self.eval_statement(&stmt, env)?;
            if let MObject::ReturnValue(v) = &*val {
                return Ok(v.clone());
            }
            value = val;
        }
        Ok(value)
    }

    fn eval_statement(&mut self, stmt: &Statement, env: &Env) -> EvalResult {
        match stmt {
            Statement::Expression(exp) => self.eval_expression(exp, env),

            Statement::BlockStatement(stmts) => {
                let mut result = self.null_obj.clone();
                for stmt in stmts {
                    let val = self.eval_statement(stmt, env)?;
                    match &*val {
                        MObject::ReturnValue(_) => return Ok(val),
                        _ => result = val,
                    };
                }
                Ok(result)
            }

            Statement::Let(ident, exp) => {
                if let Expression::Identifier(ident) = ident {
                    let val = self.eval_expression(exp, env)?;
                    env.borrow_mut().put(&ident, &val);
                    Ok(val)
                } else {
                    Err("invalid let statatemnet".to_string())
                }
            }

            Statement::Return(exp) => {
                let val = self.eval_expression(exp, env)?;
                Ok(Rc::new(MObject::ReturnValue(val)))
            }
        }
    }

    fn eval_expression(&mut self, exp: &Expression, env: &Env) -> EvalResult {
        match exp {
            Expression::IntegerLiteral(ref value) => Ok(Rc::new(MObject::Int(*value))),

            Expression::BooleanLiteral(ref value) => Ok(self.native_to_bool(*value)),

            Expression::Prefix(op, exp) => self.eval_prefix_expression(op, exp, env),

            Expression::Infix(left, op, right) => {
                let left = self.eval_expression(&left, env)?;
                let right = self.eval_expression(&right, env)?;
                self.eval_infix_expression(op, &left, &right)
            }

            Expression::If(cond, cons, alt) => {
                let cond_result = self.eval_expression(&cond, env)?;

                if self.is_truethly(&cond_result) {
                    self.eval_statement(&**cons, env)
                } else {
                    if let Some(v) = alt {
                        return self.eval_statement(v, env);
                    } else {
                        return Ok(self.null_obj.clone());
                    }
                }
            }

            Expression::Identifier(ref ident) => match env.borrow().get(ident) {
                Some(val) => Ok(val),
                None => match self.builtin.get(ident) {
                    Some(_) => Ok(Rc::new(MObject::BuiltinFunc {
                        name: ident.to_string(),
                    })),
                    None => Err(format!("identifier not found: {:?}", ident).to_string()),
                },
            },

            Expression::FunctionLiteral(params, ref bstmt) => Ok(Rc::new(MObject::Function {
                params: params.clone(),
                body: bstmt.clone(),
                env: env.clone(),
            })),
            Expression::Call(ident, params) => {
                let ident_unbox = &**ident;
                match ident_unbox {
                    Expression::Identifier(ref idkey) if idkey == "quote" => {
                        self.quote(&params[0], env)
                    }
                    _ => {
                        let func = self.eval_expression(ident, env)?;
                        let args = self.eval_expressions(params, env)?;
                        self.apply_function(&func, args.as_slice())
                    }
                }
            }

            Expression::StringLiteral(val) => Ok(Rc::new(MObject::Str(val.to_string()))),

            Expression::ArrayLiteral(vals) => {
                let mut vec = vec![];
                for val in vals {
                    vec.push(self.eval_expression(val, env)?);
                }
                Ok(Rc::new(MObject::Array(vec)))
            }

            Expression::HashLiteral(vals) => {
                let mut map = FnvHashMap::default();
                for (k, v) in vals {
                    let kv = self.eval_expression(k, env)?;
                    let value = self.eval_expression(v, env)?;
                    let key = Hashable::from(&kv)?;
                    map.insert(key, value);
                }
                Ok(Rc::new(MObject::HashMap(map)))
            }

            Expression::Index(left, index) => {
                let array = self.eval_expression(&*left, env)?;
                let index = self.eval_expression(&*index, env)?;
                Ok(self.eval_index_expression(&*array, &*index)?)
            }

            _ => Err(format!("not supported expression: {:?}", exp).to_string()),
        }
    }

    fn is_truethly(&self, obj: &MObject) -> bool {
        match obj {
            MObject::Bool(false) | MObject::Null => false,
            _ => true,
        }
    }

    fn eval_index_expression(&mut self, array: &MObject, index: &MObject) -> EvalResult {
        match (array, index) {
            (MObject::Array(vals), MObject::Int(index)) => {
                if *index < 0 || *index >= vals.len() as i64 {
                    return Ok(self.null_obj.clone());
                }
                let index = *index as usize;
                Ok(vals[index].clone())
            }

            (MObject::HashMap(map), kv) => {
                let key = Hashable::from(kv)?;
                match map.get(&key) {
                    Some(v) => Ok(v.clone()),
                    None => Ok(self.null_obj.clone()),
                }
            }
            _ => Err(format!("index operator not supported: {}", array).to_string()),
        }
    }

    fn eval_prefix_expression(&mut self, op: &PreOp, exp: &Expression, env: &Env) -> EvalResult {
        let obj = self.eval_expression(exp, env)?;

        match op {
            PreOp::Not => self.eval_bang_expression(&*obj),
            PreOp::Neg => self.eval_minus_prefix_expression(&*obj),
        }
    }

    fn eval_bang_expression(&self, right: &MObject) -> EvalResult {
        let val = match right {
            MObject::Bool(true) => self.native_to_bool(false),
            MObject::Bool(false) => self.native_to_bool(true),
            MObject::Null => self.native_to_bool(true),
            _ => self.native_to_bool(false),
        };

        Ok(val)
    }

    fn eval_minus_prefix_expression(&self, right: &MObject) -> EvalResult {
        match right {
            MObject::Int(value) => Ok(Rc::new(MObject::Int(-value))),
            _ => Err(format!("unknown operator: -{:?}", right).to_string()),
        }
    }

    fn eval_infix_expression(
        &mut self,
        op: &InfixOp,
        left: &MObject,
        right: &MObject,
    ) -> EvalResult {
        match &(left, right) {
            (MObject::Int(l), MObject::Int(r)) => self.eval_integer_infix_expression(op, *l, *r),
            (MObject::Str(l), MObject::Str(r)) => self.eval_string_infix_expression(op, l, r),
            _ => match op {
                InfixOp::Eq => Ok(self.native_to_bool(left == right)),
                InfixOp::Neq => Ok(self.native_to_bool(left != right)),
                _ => Err(format!("unknown operator: {:?} {:?} {:?}", op, left, right).to_string()),
            },
        }
    }

    fn eval_integer_infix_expression(&mut self, op: &InfixOp, left: i64, right: i64) -> EvalResult {
        match op {
            InfixOp::Add => Ok(Rc::new(MObject::Int(left + right))),
            InfixOp::Sub => Ok(Rc::new(MObject::Int(left - right))),
            InfixOp::Mul => Ok(Rc::new(MObject::Int(left * right))),
            InfixOp::Div => Ok(Rc::new(MObject::Int(left / right))),
            InfixOp::Lt => Ok(self.native_to_bool(left < right)),
            InfixOp::Gt => Ok(self.native_to_bool(left > right)),
            InfixOp::Eq => Ok(self.native_to_bool(left == right)),
            InfixOp::Neq => Ok(self.native_to_bool(left != right)),
        }
    }

    fn eval_string_infix_expression(
        &mut self,
        op: &InfixOp,
        left: &str,
        right: &str,
    ) -> EvalResult {
        match op {
            InfixOp::Add => Ok(Rc::new(MObject::Str(
                format!("{}{}", left, right).to_string(),
            ))),
            InfixOp::Lt => Ok(self.native_to_bool(left < right)),
            InfixOp::Gt => Ok(self.native_to_bool(left > right)),
            InfixOp::Eq => Ok(self.native_to_bool(left == right)),
            InfixOp::Neq => Ok(self.native_to_bool(left != right)),
            _ => Err(format!("unknown operator: {:?} for str", op)),
        }
    }

    fn eval_expressions(
        &mut self,
        args: &[Expression],
        env: &Env,
    ) -> Result<Vec<Rc<MObject>>, String> {
        let mut vec = Vec::with_capacity(args.len());
        for arg in args {
            vec.push(self.eval_expression(arg, env)?);
        }

        Ok(vec)
    }

    fn apply_function(&mut self, func: &MObject, args: &[Rc<MObject>]) -> EvalResult {
        match func {
            MObject::Function {
                ref params,
                ref body,
                ref env,
            } => {
                let newenv = Rc::new(RefCell::new(Environment::enclose_env(&env)));

                for (arg, param) in args.iter().zip(params.iter()) {
                    if let Expression::Identifier(key) = &param {
                        newenv.borrow_mut().put(key, arg);
                    }
                }
                let value = self.eval_statement(body, &newenv)?;
                Ok(self.unwrap_return_value(value))
            }
            MObject::BuiltinFunc { ref name } => {
                let func = &self.builtin[name];
                let value = func(args)?;

                Ok(self.unwrap_return_value(value))
            }
            _ => unreachable!(),
        }
    }

    fn unwrap_return_value(&self, val: Rc<MObject>) -> Rc<MObject> {
        if let MObject::ReturnValue(v) = &*val {
            return v.clone();
        }
        val
    }

    fn native_to_bool(&self, val: bool) -> Rc<MObject> {
        if val {
            self.true_obj.clone()
        } else {
            self.false_obj.clone()
        }
    }

    fn quote(&mut self, expr: &Expression, env: &Env) -> EvalResult {
        let ast = self.eval_unquote_calls(expr, env)?;
        Ok(Rc::new(MObject::Quote(Box::new(ast))))
    }

    fn eval_unquote_calls(&mut self, expr: &Expression, env: &Env) -> Result<Expression, String> {
        let mut modifier = |expr: Expression| -> Result<Expression, String> {
            match &expr {
                Expression::Call(ident, params) if params.len() == 1 => {
                    let ident_unbox = &**ident;
                    match ident_unbox {
                        Expression::Identifier(ref idkey) if idkey == "unquote" => {
                            let unquote = self.eval_expression(&params[0], env)?;
                            Ok(self.convert_mobject_to_ast(&*unquote))
                        }
                        _ => Ok(expr.clone()),
                    }
                }
                _ => Ok(expr.clone()),
            }
        };

        modify_expression(expr.clone(), &mut modifier)
    }

    fn convert_mobject_to_ast(&self, obj: &MObject) -> Expression {
        match obj {
            MObject::Int(val) => Expression::IntegerLiteral(*val),
            MObject::Bool(val) => Expression::BooleanLiteral(*val),
            MObject::Quote(val) => *val.clone(),
            _ => panic!("unexpect unquote object {:?}", obj),
        }
    }

    pub fn define_macros(&mut self, program: &Program, env: &Env) -> Result<Program, String> {
        let macro_stmts: Vec<_> = program
            .statements
            .iter()
            .filter(|stmt| stmt.is_macro())
            .collect();

        for stmt in &macro_stmts {
            self.add_macro(stmt, env);
        }

        let stmts: Vec<_> = program
            .statements
            .iter()
            .filter(|stmt| !stmt.is_macro())
            .cloned()
            .collect();

        Ok(Program { statements: stmts })
    }

    fn add_macro(&self, stmt: &Statement, env: &Env) {
        match stmt {
            Statement::Let(ident, value) => match (ident, value) {
                (Expression::Identifier(ident), Expression::MacroLiteral(params, body)) => {
                    let obj = Rc::new(MObject::Macro {
                        params: params.to_vec(),
                        body: Box::new(*body.clone()),
                        env: env.clone(),
                    });
                    env.borrow_mut().put(ident, &obj);
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    pub fn expand_macro(&mut self, program: &Program, env: &Env) -> Result<Program, String> {
        let mut modifier = |expr: Expression| -> Result<Expression, String> {
            match &expr {
                Expression::Call(ref ident, ref args) => {
                    let m = self.is_macro_call(&**ident, &env);
                    if m.is_none() {
                        return Ok(expr.clone());
                    }

                    let qargs = self.quote_args(&args);
                    let m = m.unwrap();
                    let eval_env = self.extended_macro_env(&m, &qargs);

                    match &*m {
                        MObject::Macro {
                            params: _, body, ..
                        } => {
                            let evaluated = self.eval_statement(&body, &eval_env);
                            if evaluated.is_ok() {
                                let evaluated = evaluated.unwrap();
                                if let MObject::Quote(ref quote) = *evaluated {
                                    return Ok(*quote.clone());
                                } else {
                                    return Ok(expr.clone());
                                }
                            } else {
                                Ok(expr.clone())
                            }
                        }
                        _ => Ok(expr.clone()),
                    }
                }
                _ => Ok(expr.clone()),
            }
        };

        modify(program.clone(), &mut modifier)
    }

    fn is_macro_call(&self, ident: &Expression, env: &Env) -> Option<Rc<MObject>> {
        match &ident {
            Expression::Identifier(ref name) => env.borrow().get(&*name),
            _ => None,
        }
    }

    fn quote_args(&self, args: &[Expression]) -> Vec<Rc<MObject>> {
        args.iter()
            .map(|arg| Rc::new(MObject::Quote(Box::new(arg.clone()))))
            .collect()
    }

    fn extended_macro_env(&self, macro_obj: &Rc<MObject>, args: &[Rc<MObject>]) -> Env {
        match &**macro_obj {
            MObject::Macro {
                ref params,
                body: _,
                ref env,
            } => {
                let newenv = Rc::new(RefCell::new(Environment::enclose_env(&env)));

                for (i, arg) in args.iter().enumerate() {
                    if let Expression::Identifier(key) = &params[i] {
                        newenv.borrow_mut().put(key, arg);
                    }
                }

                newenv
            }
            _ => panic!("unexpected object {:?}", macro_obj),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::Lexer;
    use object::MObject;
    use parser::Parser;

    fn test_eval(input: &str) -> EvalResult {
        let l = Lexer::with_string(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program().unwrap();
        let env = Rc::new(RefCell::new(Environment::new()));
        let mut eval = Evaluator::new();
        eval.eval(&program, &env)
    }

    fn test_boolean(obj: &MObject, value: bool) {
        match obj {
            MObject::Bool(v) => assert_eq!(*v, value),
            _ => panic!("object isn't boolean: {:?}", obj),
        }
    }

    fn test_integer(obj: &MObject, value: i64) {
        match obj {
            MObject::Int(v) => assert_eq!(*v, value),
            _ => panic!("object isn't integer: {:?}", obj),
        }
    }

    fn test_string(obj: &MObject, value: &str) {
        match obj {
            MObject::Str(v) => assert_eq!(*v, value),
            _ => panic!("object isn't str: {:?}", obj),
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = [
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == true", false),
            ("true == false", false),
            ("true != true", false),
            ("false != false", false),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == false", true),
            ("(1 > 2) == true", false),
        ];

        for t in tests.into_iter() {
            let result = test_eval(t.0).unwrap();
            test_boolean(&*result, t.1);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = [
            ("true", true),
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for t in tests.into_iter() {
            let result = test_eval(t.0).unwrap();
            test_boolean(&*result, t.1);
        }
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = [
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 - 50", 0),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for t in tests.into_iter() {
            let result = test_eval(t.0).unwrap();
            test_integer(&*result, t.1);
        }
    }

    #[test]
    fn test_eval_if_expression() {
        let tests = [
            ("if(true){10}", MObject::Int(10)),
            ("if(false){10}", MObject::Null),
            ("if(1){10}", MObject::Int(10)),
            ("if(1 < 2){10}", MObject::Int(10)),
            ("if(1 > 2){10}", MObject::Null),
            ("if(1 > 2){10} else {20}", MObject::Int(20)),
            ("if(1 < 2){10} else {20}", MObject::Int(10)),
        ];

        for t in tests.into_iter() {
            let result = test_eval(t.0).unwrap();

            match (&*result, &t.1) {
                (MObject::Int(_), MObject::Int(expected)) => test_integer(&*result, *expected),
                _ => assert_eq!(*result, t.1),
            }
        }
    }

    #[test]
    fn test_result_statements() {
        let tests = [
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                r#"
if (10 > 1) {
    if (10 > 1) {
        return 10;
    }
    return 1;
} "#,
                10,
            ),
        ];

        for t in tests.into_iter() {
            let result = test_eval(t.0).unwrap();
            match &*result {
                MObject::Int(_) => test_integer(&*result, t.1),
                _ => panic!("unexpected type: {:?}", result),
            }
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = [
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];
        for t in tests.into_iter() {
            let result = test_eval(t.0).unwrap();

            match &*result {
                MObject::Int(_) => test_integer(&*result, t.1),
                _ => panic!("unexpected type: {:?}", &*result),
            }
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; }";

        let result = test_eval(input).unwrap();

        match &*result {
            MObject::Function {
                ref params,
                ref body,
                ..
            } => {
                assert_eq!(1, params.len());
                if let Expression::Identifier(ref x) = params[0] {
                    assert_eq!("x", x);
                } else {
                    panic!("unexpected param type: {:?}", params[0]);
                }

                match &**body {
                    Statement::BlockStatement(stmts) => {
                        assert_eq!(1, stmts.len());
                    }
                    _ => panic!("unexpected type: {:?}", body),
                }
            }
            _ => panic!("unexpected type: {:?}", &*result),
        }
    }

    #[test]
    fn test_function_app() {
        let tests = [
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
            ("let a = fn(w) { fn(z) {w - z}; };let b = a(5);b(7);", -2),
        ];
        for t in tests.into_iter() {
            let result = test_eval(t.0).unwrap();
            match &*result {
                MObject::Int(_) => test_integer(&*result, t.1),
                _ => panic!("unexpected type: {:?}", result),
            }
        }
    }

    #[test]
    fn test_string_literal_expression() {
        let input = r#""hello world""#;
        let result = test_eval(input).unwrap();

        match &*result {
            MObject::Str(_) => test_string(&*result, "hello world"),
            _ => panic!("unexpected type: {:?}", result),
        }
    }

    #[test]
    fn test_string_concatination() {
        let input = r#""Hello" + " " + "World" "#;
        let result = test_eval(input).unwrap();

        match &*result {
            MObject::Str(_) => test_string(&*result, "Hello World"),
            _ => panic!("unexpected type: {:?}", result),
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";

        let result = test_eval(input).unwrap();

        match &*result {
            MObject::Array(vals) => {
                test_integer(&*vals[0], 1);
                test_integer(&*vals[1], 4);
                test_integer(&*vals[2], 6);
            }
            _ => panic!("unexpected type: {:?}", result),
        }
    }

    #[test]
    fn test_array_index_expressions() {
        let tests = [
            ("[1, 2, 3][0]", 1),
            ("[1, 2, 3][1]", 2),
            ("[1, 2 ,3][2]", 3),
            ("let i = 0; [1][i];", 1),
            ("let myArray = [1, 2, 3]; myArray[2];", 3),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                6,
            ),
            ("let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", 2),
        ];

        for test in &tests {
            let result = test_eval(test.0).unwrap();
            match &*result {
                MObject::Int(_) => {
                    test_integer(&*result, test.1);
                }
                _ => panic!("unexpected type: {:?}", result),
            }
        }
    }

    #[test]
    fn test_hash_literals() {
        let input = r#"let two = "two";
{
           "one": 10 - 9,
           two: 1 + 1,
           "thr" + "ee": 6 / 2,
           4: 4,
           true: 5,
           false: 6
}"#;

        let result = test_eval(input).unwrap();
        match &*result {
            MObject::HashMap(map) => {
                assert_eq!(6, map.len());
                assert_eq!(
                    MObject::Int(1),
                    **map.get(&Hashable::from_str("one")).unwrap()
                );
                assert_eq!(
                    MObject::Int(2),
                    **map.get(&Hashable::from_str("two")).unwrap()
                );
                assert_eq!(
                    MObject::Int(3),
                    **map.get(&Hashable::from_str("three")).unwrap()
                );
                assert_eq!(MObject::Int(4), **map.get(&Hashable::Int(4)).unwrap());
                assert_eq!(MObject::Int(5), **map.get(&Hashable::Bool(true)).unwrap());
                assert_eq!(MObject::Int(6), **map.get(&Hashable::Bool(false)).unwrap());
            }
            _ => panic!("unexpected type: {:?}", result),
        }
    }

    #[test]
    fn test_hash_index() {
        let tests = [
            (r#"{"foo":5}["foo"]"#, MObject::Int(5)),
            (r#"{"foo":5}["bar"]"#, MObject::Null),
            (r#"let key="foo";{"foo":6}[key];"#, MObject::Int(6)),
        ];

        for t in &tests {
            let result = test_eval(t.0).unwrap();

            match &*result {
                MObject::Int(_) => assert_eq!(*result, t.1),
                MObject::Null => assert_eq!(*result, MObject::Null),
                _ => panic!("test failure"),
            }
        }
    }

    #[test]
    fn test_define_macros() {
        let input = r#"
let number = 1;
let function = fn(x, y) { x + y; };
let mymacro = macro(x, y) {x + y; };
"#;
        let l = Lexer::with_string(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program().unwrap();
        let env = Rc::new(RefCell::new(Environment::new()));
        let mut eval = Evaluator::new();
        let program = eval.define_macros(&program, &env).unwrap();

        assert_eq!(2, program.statements.len());
        assert_eq!(None, env.borrow().get("number"));
        assert_eq!(None, env.borrow().get("function"));

        let a = env.borrow().get("mymacro");
        match a {
            Some(m) => match *m {
                MObject::Macro {
                    ref params,
                    ref body,
                    ..
                } => {
                    assert_eq!(2, params.len());
                }
                _ => panic!("object is not macro: {:?}", m),
            },
            None => panic!("macro not in environment."),
        }
    }

    #[test]
    fn test_expand_macros() {
        let input = r#"
let unless_macro = macro(condition, consequence, alternative) {
    quote(if (!(unquote(condition))) {
        unquote(consequence);
    } else {
        unquote(alternative);
    });
};

unless_macro(10 > 5, puts("not greater"), puts("greater"));

"#;
        let l = Lexer::with_string(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program().unwrap();
        let env = Rc::new(RefCell::new(Environment::new()));
        let mut eval = Evaluator::new();
        let program = eval.define_macros(&program, &env).unwrap();
        let program = eval.expand_macro(&program, &env).unwrap();
        println!("program {:?}", program);
    }
}
