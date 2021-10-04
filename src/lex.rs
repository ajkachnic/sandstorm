use ariadne::{Label, Report, ReportKind};
use std::iter::Peekable;
use std::str::Chars;
use thiserror::Error;

use crate::{
    ast::{Span, Spanned},
    cache::FileCache,
    source::Source,
};

pub type SpannedToken = Spanned<TokenKind>;

#[derive(Clone, Error, Debug)]
pub enum LexError {
    #[error("Expected end of string. Check to make sure you closed all of your quotes")]
    ExpectedEndOfString { src: Source, bad: Span },

    #[error("EOF")]
    EndOfFile,

    #[error("Unexpected character {ch}")]
    UnexpectedCharacter { ch: char },
}

pub type LexResult<T> = Result<T, LexError>;

#[derive(Clone, Debug)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftSquareBracket,
    RightSquareBracket,
    LeftCurlyBrace,
    RightCurlyBrace,
    Dot,
    Assign,
    Operator(Operator),

    Newline, // Ignored sometimes but useful in other cases

    // Literals
    Int(isize),
    Bool(bool),
    String(String),
    Identifier(String),

    // Keywords
    Func,
    Import,
    As,
    Void,
    Return,
    If,
    Else,
    Let,
    Const,
}

#[derive(Clone, Debug, Copy)]
pub enum Operator {
    // Comparison
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Plus,
    Minus,
    Asterisk,
    Slash,
}

impl From<String> for TokenKind {
    fn from(str: String) -> Self {
        match str.as_str() {
            "as" => TokenKind::As,
            "const" => TokenKind::Const,
            "else" => TokenKind::Else,
            "false" => TokenKind::Bool(false),
            "func" => TokenKind::Func,
            "if" => TokenKind::If,
            "import" => TokenKind::Import,
            "let" => TokenKind::Let,
            "return" => TokenKind::Return,
            "true" => TokenKind::Bool(true),
            "void" => TokenKind::Void,

            _ => TokenKind::Identifier(str),
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
        return false;
    }

    pub fn next_token(&mut self) -> LexResult<SpannedToken> {
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
                '.' => TokenKind::Dot,
                '>' => {
                    if self.peek_consume('=') {
                        TokenKind::Operator(Operator::GreaterEqual)
                    } else {
                        TokenKind::Operator(Operator::Greater)
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
                    } else {
                        TokenKind::Assign
                    }
                }
                '+' => TokenKind::Operator(Operator::Plus),
                '-' => TokenKind::Operator(Operator::Minus),
                '*' => TokenKind::Operator(Operator::Asterisk),
                '/' => TokenKind::Operator(Operator::Slash),
                '\n' => TokenKind::Newline,
                'a'..='z' | 'A'..='Z' => self.lex_ident(ch),
                '0'..='9' => self.lex_number(ch),
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
    fn lex_number(&mut self, ch: char) -> TokenKind {
        let mut n = String::from(ch);
        while let Some(ch) = self.peek() {
            match ch {
                '0'..='9' => n.push(*ch),
                _ => break,
            }
            self.consume();
        }

        TokenKind::Int(n.parse().unwrap())
    }

    fn lex_string(&mut self) -> LexResult<TokenKind> {
        let mut str = String::new();
        let start = self.pos;
        while let Some(ch) = self.consume() {
            if ch == '"' {
                return Ok(TokenKind::String(str));
            }
            str.push(ch)
        }

        Err(LexError::ExpectedEndOfString {
            bad: (start, self.pos - start),
            src: self.src.clone(),
        })
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            match ch {
                ' ' | '\t' | '\r' => self.consume(),
                _ => break,
            };
        }
    }

    pub fn tokens(&mut self) -> LexResult<Vec<SpannedToken>> {
        let mut toks = Vec::new();

        loop {
            match self.next_token() {
                Ok(tok) => toks.push(tok),
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
            src: Source::Text(std::rc::Rc::new(source.to_string())),
            pos: 0,
        }
    }
}
