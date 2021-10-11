use miette::Diagnostic;
use serde::{Deserialize, Serialize};
use std::iter::Peekable;
use std::str::Chars;
use thiserror::Error;

use crate::{
    ast::{Span, Spanned},
    cache::FileCache,
    source::Source,
};

pub type SpannedToken = Spanned<TokenKind>;

#[derive(Clone, Error, Debug, Diagnostic)]
pub enum LexError {
    #[error("Expected end of string. Check to make sure you closed all of your quotes")]
    ExpectedEndOfString { src: Source, bad: Span },

    #[error("Expected end of char literal. Check to make sure you closed all of your quotes")]
    ExpectedEndOfChar { src: Source, bad: Span },

    #[error("EOF")]
    EndOfFile,

    #[error("Unexpected character {ch}")]
    UnexpectedCharacter { ch: char },
}

pub type LexResult<T> = Result<T, LexError>;

#[derive(Clone, Debug, PartialEq, Copy, Serialize, Deserialize)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftSquareBracket,
    RightSquareBracket,
    LeftCurlyBrace,
    RightCurlyBrace,
    Dot,
    Colon,
    Semicolon, // Rarely used explicitly, usually infered
    Assign,
    Comma,
    Bang,
    Question,
    Operator(Operator),
    DoubleArrow,

    // Literals
    NumberLiteral,
    StringLiteral,
    CharLiteral,
    Identifier,
    True,
    False,

    // Keywords
    And,
    As,
    /// The keyword bool, as in `bool`, not the value
    Bool,
    Const,
    Else,
    For,
    Func,
    If,
    Import,
    In,
    /// The keyword `int`, not a number
    Int,
    Let,
    Nil,
    Not,
    Or,
    Return,
    String,
    Switch,
    Try,
    Type,
    Void,
}

#[derive(Clone, Debug, Copy, PartialEq, Serialize, Deserialize)]
pub enum Operator {
    // Comparison
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Plus,
    Minus,
    Asterisk,
    Slash,

    PlusEqual,
    MinusEqual,
    AsteriskEqual,
    SlashEqual,
}

impl From<String> for TokenKind {
    fn from(str: String) -> Self {
        match str.as_str() {
            "and" => TokenKind::And,
            "as" => TokenKind::As,
            "bool" => TokenKind::Bool,
            "const" => TokenKind::Const,
            "else" => TokenKind::Else,
            "false" => TokenKind::False,
            "for" => TokenKind::For,
            "func" => TokenKind::Func,
            "if" => TokenKind::If,
            "import" => TokenKind::Import,
            "in" => TokenKind::In,
            "int" => TokenKind::Int,
            "let" => TokenKind::Let,
            "nil" => TokenKind::Nil,
            "not" => TokenKind::Not,
            "or" => TokenKind::Or,
            "return" => TokenKind::Return,
            "string" => TokenKind::String,
            "switch" => TokenKind::Switch,
            "true" => TokenKind::True,
            "try" => TokenKind::Try,
            "type" => TokenKind::Type,
            "void" => TokenKind::Void,

            _ => TokenKind::Identifier,
        }
    }
}

pub struct Lexer<'a> {
    iter: Peekable<Chars<'a>>,
    // A copy of the source code, used for error handling
    src: Source,
    pos: usize,
}

impl<'a> Lexer<'a> {
    fn consume(&mut self) -> Option<char> {
        self.pos += 1;
        self.iter.next()
    }
    fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }
    fn peek_is(&mut self, check: char) -> bool {
        return self.peek() == Some(&check);
    }

    fn peek_consume(&mut self, check: char) -> bool {
        if self.peek() == Some(&check) {
            self.consume();
            return true;
        }
        false
    }

    fn next_token(&mut self) -> LexResult<SpannedToken> {
        self.skip_whitespace();
        let initial_pos = self.pos;
        if let Some(ch) = self.consume() {
            let kind = match ch {
                '(' => TokenKind::LeftParen,
                ')' => TokenKind::RightParen,
                '{' => TokenKind::LeftCurlyBrace,
                '}' => TokenKind::RightCurlyBrace,
                '[' => TokenKind::LeftSquareBracket,
                ']' => TokenKind::RightSquareBracket,
                ':' => TokenKind::Colon,
                '.' => TokenKind::Dot,
                ',' => TokenKind::Comma,
                '?' => TokenKind::Question,
                '>' => {
                    if self.peek_consume('=') {
                        TokenKind::Operator(Operator::GreaterEqual)
                    } else {
                        TokenKind::Operator(Operator::Greater)
                    }
                }
                '!' => {
                    if self.peek_consume('=') {
                        TokenKind::Operator(Operator::NotEqual)
                    } else {
                        TokenKind::Bang
                    }
                }
                '<' => {
                    if self.peek_consume('=') {
                        TokenKind::Operator(Operator::LessEqual)
                    } else {
                        TokenKind::Operator(Operator::Less)
                    }
                }
                '=' => {
                    if self.peek_consume('=') {
                        TokenKind::Operator(Operator::Equal)
                    } else if self.peek_consume('>') {
                        TokenKind::DoubleArrow
                    } else {
                        TokenKind::Assign
                    }
                }
                '+' => {
                    if self.peek_consume('=') {
                        TokenKind::Operator(Operator::PlusEqual)
                    } else {
                        TokenKind::Operator(Operator::Plus)
                    }
                }
                '-' => {
                    if self.peek_consume('=') {
                        TokenKind::Operator(Operator::MinusEqual)
                    } else {
                        match self.peek() {
                            Some('0'..='9') => {
                                self.consume();
                                self.lex_number()
                            }
                            _ => TokenKind::Operator(Operator::Minus),
                        }
                    }
                }
                '*' => TokenKind::Operator(Operator::Asterisk),
                '/' => TokenKind::Operator(Operator::Slash),
                'a'..='z' | 'A'..='Z' => self.lex_ident(ch),
                '0'..='9' => self.lex_number(),
                '\'' => self.lex_char()?,
                '"' => self.lex_string()?,
                ch => return Err(LexError::UnexpectedCharacter { ch }),
            };

            let end_pos = self.pos;

            return Ok(Spanned::builder()
                .node(kind)
                .span((initial_pos, end_pos))
                .build());
        }

        Err(LexError::EndOfFile)
    }

    fn lex_ident(&mut self, ch: char) -> TokenKind {
        let mut ident = String::from(ch);
        while let Some(ch) = self.peek() {
            match ch {
                'a'..='z' | 'A'..='Z' | '0'..='9' => ident.push(*ch),
                _ => break,
            }
            self.consume();
        }

        TokenKind::from(ident)
    }

    // Currently only supports integers
    fn lex_number(&mut self) -> TokenKind {
        while let Some(ch) = self.peek() {
            match ch {
                '0'..='9' => {
                    self.consume();
                }
                _ => break,
            }
        }

        TokenKind::NumberLiteral
    }

    fn lex_string(&mut self) -> LexResult<TokenKind> {
        let start = self.pos;
        while let Some(ch) = self.consume() {
            if ch == '"' {
                return Ok(TokenKind::StringLiteral);
            }
        }

        Err(LexError::ExpectedEndOfString {
            bad: (start, self.pos - start),
            src: self.src.clone(),
        })
    }

    fn lex_char(&mut self) -> LexResult<TokenKind> {
        let start = self.pos;
        // TODO: Unicode support
        if let Some(ch) = self.consume() {
            if ch == '\'' {
                return Ok(TokenKind::CharLiteral);
            }
        }

        if self.consume() == Some('\'') {
            return Ok(TokenKind::CharLiteral);
        }

        Err(LexError::ExpectedEndOfChar {
            bad: (start, self.pos - start),
            src: self.src.clone(),
        })
    }

    fn skip_whitespace(&mut self) -> Option<TokenKind> {
        while let Some(ch) = self.peek() {
            match ch {
                ' ' | '\t' | '\r' | '\n' => self.consume(),
                _ => break,
            };
        }
        None
    }

    pub fn tokens(&mut self) -> LexResult<Vec<SpannedToken>> {
        let mut toks = Vec::new();

        loop {
            match self.next_token() {
                Ok(tok) => {
                    toks.push(tok);
                    if self.peek() == Some(&'\n')
                        && matches!(
                            tok.node,
                            TokenKind::Identifier
                                | TokenKind::NumberLiteral
                                | TokenKind::CharLiteral
                                | TokenKind::StringLiteral
                                | TokenKind::Return
                                | TokenKind::RightParen
                                | TokenKind::RightSquareBracket
                                | TokenKind::RightCurlyBrace
                        )
                    {
                        toks.push(
                            Spanned::builder()
                                .node(TokenKind::Semicolon)
                                .span((self.pos, self.pos + 1))
                                .build(),
                        );
                    }
                }
                Err(e) => {
                    if matches!(e, LexError::EndOfFile) {
                        return Ok(toks);
                    } else {
                        return Err(e);
                    }
                }
            }
        }
    }

    pub fn from_file(cache: &mut FileCache, path: &'a str) -> LexResult<Vec<SpannedToken>> {
        let file = cache.load(path).unwrap();

        let mut lexer = Lexer {
            pos: 0,
            src: Source::File(path.into()),
            iter: file.as_str().chars().peekable(),
        };

        lexer.tokens()
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = SpannedToken;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(t) => Some(t),
            Err(_) => None,
        }
    }
}

impl<'a> From<&'a str> for Lexer<'a> {
    fn from(source: &'a str) -> Self {
        Lexer {
            iter: source.chars().peekable(),
            src: Source::Text(std::sync::Arc::new(source.to_string())),
            pos: 0,
        }
    }
}

#[test]
fn test_lex() {
    let mut lexer: Lexer = "func fib() void {
        return 1 => 2
    }"
    .into();

    let tokens = lexer.tokens().unwrap();
    insta::assert_yaml_snapshot!(tokens);
}
