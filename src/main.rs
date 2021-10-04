mod ast;
mod cache;
mod checker;
mod lex;
mod parse;
mod source;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut file_cache = cache::FileCache::new();

    let tokens = lex::Lexer::from_file(&mut file_cache, "samples/fib.storm")?;

    Ok(())
}
