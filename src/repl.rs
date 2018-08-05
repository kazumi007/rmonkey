use evaluator::*;
use lexer::Lexer;
use object::Environment;
use parser::Parser;
use std::cell::RefCell;
use std::io::{Stdin, Write};
use std::rc::Rc;

const PROMPT: &str = ">> ";

pub fn start_repl(reader: &mut Stdin, writer: &mut Write) {
    let env = Rc::new(RefCell::new(Environment::new()));
    loop {
        write!(writer, "{}", PROMPT);
        writer.flush();
        let mut buf = String::new();
        reader.read_line(&mut buf);
        let l = Lexer::with_string(&buf);
        let mut p = Parser::new(l);
        let mut e = Evaluator::new();
        let result = p.parse_program();

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
    let l = Lexer::with_string(&buf);
    let mut p = Parser::new(l);
    let mut e = Evaluator::new();
    let result = p.parse_program();
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
