mod ast;
mod cache;
mod checker;
mod lex;
mod parse;
mod source;

use getopts::Options;
use std::env;

fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} FILE [options]", program);
    println!("{}", opts.usage(&brief))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut file_cache = cache::FileCache::new();

    let mut args = env::args();
    let program = args.next().unwrap();
    let args: Vec<String> = args.collect();

    let mut opts = Options::new();
    let matches = opts.parse(args)?;

    let file_path = if !matches.free.is_empty() {
        matches.free[0].clone()
    } else {
        print_usage(&program, opts);
        return Ok(());
    };

    let tokens = lex::Lexer::from_file(&mut file_cache, &file_path)?;
    println!("{:?}", tokens);

    let mut parser = parse::Parser::new(tokens);
    let program = parser.parse_program()?;

    println!("{:?}", program);

    Ok(())
}
