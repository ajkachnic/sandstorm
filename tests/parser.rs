use std::sync::Arc;

use sandstorm::lex;
use sandstorm::parse;
use sandstorm::source::Source;

#[test]
fn test_import() {
    let mut cache = sandstorm::cache::FileCache::new();
    let src = "import std as io
import std.mem.free";
    let source = Source::Text(Arc::new(src.to_string()));
    let tokens = lex::Lexer::from(src).tokens().unwrap();

    let mut parser = parse::Parser::new(tokens, source, &mut cache);
    let program = parser.parse_program().unwrap();

    insta::assert_yaml_snapshot!(program);
}

#[test]
fn test_if() {
    let mut cache = sandstorm::cache::FileCache::new();
    let src = "if true and (1 == 1 or false) {
        foo()
    } else if 2 < 3 {
        bar()
    } else {
        baz()
    }";
    let source = Source::Text(Arc::new(src.to_string()));
    let tokens = lex::Lexer::from(src).tokens().unwrap();

    let mut parser = parse::Parser::new(tokens, source, &mut cache);
    let program = parser.parse_program().unwrap();

    insta::assert_yaml_snapshot!(program);
}
