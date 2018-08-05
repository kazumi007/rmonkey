extern crate rmonkey;

use rmonkey::repl;
use std::env;
use std::fs::File;
use std::io;
use std::io::Read;
use std::io::{BufReader};

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().skip(1).collect();
    if args.is_empty() {
        repl::start_repl(&mut io::stdin(), &mut io::stdout());
    } else {
        let f = File::open(&args[0])?;
        let mut reader = BufReader::new(f);

        let mut buf = Vec::new();
        reader.read_to_end(&mut buf);
        let str1 = std::str::from_utf8(&buf).unwrap();
        repl::start_interpreter(str1, &mut io::stdout());
    }
    Ok(())
}
