use evaluator::*;
use lexer::Lexer;
use object::Environment;
use parser::Parser;
use std::cell::RefCell;
use std::io::{Stdin, Write};
use std::rc::Rc;
use ast::Program;

const PROMPT: &str = ">> ";

pub fn start_repl(reader: &mut Stdin, writer: &mut Write) {
    let env = Rc::new(RefCell::new(Environment::new()));
    let macro_env = Rc::new(RefCell::new(Environment::new()));
    loop {
        write!(writer, "{}", PROMPT);
        writer.flush();
        let mut buf = String::new();
        reader.read_line(&mut buf);
        let mut l = Lexer::with_string(&buf);
        let mut p = Parser::new(&mut l);
        let mut e = Evaluator::new();
        let result = parse_and_expand_macro(&mut p, &mut e, &macro_env);

        match result {
            Ok(program) => {
                let result = e.eval(&program, &env);
                match result {
                    Ok(opt) => writeln!(writer, "{}", &*opt),
                    Err(err) => writeln!(writer, "{}", err),
                };
            }
            Err(err) => {
                writeln!(writer, "{}", err);
            }
        }
    }
}

pub fn start_interpreter(buf: &str, writer: &mut Write) {
    let env = Rc::new(RefCell::new(Environment::new()));
    let macro_env = Rc::new(RefCell::new(Environment::new()));

    let mut l = Lexer::with_string(&buf);
    let mut p = Parser::new(&mut l);
    let mut e = Evaluator::new();
    let result = parse_and_expand_macro(&mut p, &mut e, &macro_env);

    match result {
        Ok(program) => {
            let result = e.eval(&program, &env);
            match result {
                Ok(opt) => writeln!(writer, "{}", &*opt),
                Err(err) => writeln!(writer, "{}", err),
            };
        }
        Err(err) => {
            writeln!(writer, "{}", err);
        }
    }
}

fn parse_and_expand_macro(p: &mut Parser, e: &mut Evaluator, env: &Env) -> Result<Program, String> {
    let program = p.parse_program()?;
    let define_program = e.define_macros(&program, env)?;
    let program = e.expand_macro(&define_program, env)?;
    Ok(program)
}
