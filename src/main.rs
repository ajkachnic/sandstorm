mod ast;
mod cache;
mod checker;
mod lex;
mod parse;
mod source;

use getopts::Options;
use std::env;

use miette::{IntoDiagnostic, Result};

fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} FILE [options]", program);
    println!("{}", opts.usage(&brief))
}

fn main() -> Result<()> {
    let mut file_cache = cache::FileCache::new();

    let mut args = env::args();
    let program = args.next().unwrap();
    let args: Vec<String> = args.collect();

    let mut opts = Options::new();
    let matches = opts.parse(args).into_diagnostic()?;

    let file_path = if !matches.free.is_empty() {
        matches.free[0].clone()
    } else {
        print_usage(&program, opts);
        return Ok(());
    };

    let file = source::Source::File((&file_path).into());
    let tokens = lex::Lexer::from_file(&mut file_cache, &file_path)?;

    for t in &tokens {
        println!("{:?}", t);
    }

    let mut parser = parse::Parser::new(tokens, file, &mut file_cache);
    let program = parser.parse_program()?;

    println!("{:?}", program);

    Ok(())
}
