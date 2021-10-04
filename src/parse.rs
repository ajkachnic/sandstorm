use crate::lex::{LexResult, Lexer, SpannedToken, TokenKind};
use thiserror::Error;

#[derive(Clone, Debug, Error)]
#[error("")]
pub enum ParseError {
    Unexpected(),
}

pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser {
    pos: usize,
    tokens: Vec<SpannedToken>,
}

impl Parser {
    pub fn new(tokens: Vec<SpannedToken>) -> Self {
        Self { pos: 0, tokens }
    }
}

impl<'a> From<Lexer<'a>> for LexResult<Parser> {
    fn from(mut l: Lexer) -> Self {
        match l.tokens() {
            Ok(toks) => Ok(Parser::new(toks)),
            Err(err) => Err(err),
        }
    }
}
