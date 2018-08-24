// #![feature(test)]
// extern crate test;
extern crate fnv;

pub mod ast;
pub mod builtins;
pub mod evaluator;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod repl;
pub mod token;

#[cfg(test)]
mod tests {
    use super::*;
    use std::rc::Rc;
    use std::cell::RefCell;
    use evaluator::*;
    use lexer::*;
    use object::*;
    use parser::*;
  //  use test::Bencher;

    fn test_eval(input: &str) -> EvalResult {
        let mut l = Lexer::with_string(input);
        let mut parser = Parser::new(&mut l);
        let program = parser.parse_program().unwrap();
        let env = Rc::new(RefCell::new(Environment::new()));
        let mut eval = Evaluator::new();
        eval.eval(&program, &env)
    }

//     #[bench]
//     fn bench_fib(b: &mut Bencher) {
//         let input = r#"
//  let fib = fn(n) {
//      if (n < 3) {
//          return 1;
//      }
//      return fib(n-1) + fib(n-2);
//  }

//  fib(20);"#;

//         b.iter(|| test_eval(input).unwrap());
//     }
}
