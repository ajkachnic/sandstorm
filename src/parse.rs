use crate::ast::{
    DeclarationKind, DeclarationStatement, Expression, ExpressionKind, FunctionStatement,
    Identifier, ImportStatement, Program, Span, Spanned, Statement, StatementKind,
};
use crate::lex::{LexResult, Lexer, SpannedToken, TokenKind};
use thiserror::Error;

#[derive(Clone, Debug, Error)]
pub enum ParseError {
    #[error("Unexpected end of file")]
    UnexpectedEndOfFile,

    #[error("Expected alias for import")]
    ImportDanglingAs,

    #[error("Expected token kind {:?}, found token {0:?}")]
    ExpectedFound(TokenKind, SpannedToken),
}

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone)]
pub struct Parser {
    pos: usize,
    tokens: Vec<SpannedToken>,
    current: Option<SpannedToken>,
    peek: Option<SpannedToken>,
    eof: bool,
}

impl Parser {
    pub fn new(tokens: Vec<SpannedToken>) -> Self {
        Self {
            pos: 1,
            eof: false,
            tokens,
            current: tokens.get(0).map(|s| s.clone()),
            peek: tokens.get(1).map(|s| s.clone()),
        }
    }

    fn span(&self) -> Span {
        match self.current {
            Some(s) => s.span,
            None => (0, 0),
        }
    }

    fn advance(&mut self) {
        self.pos += 1;
        self.current = self.peek;
        self.peek = self.tokens.get(self.pos).map(|s| s.clone());
        if self.pos >= self.tokens.len() {
            self.eof = true;
        }
    }

    fn peek_is(&self, check: TokenKind) -> bool {
        match self.peek {
            Some(tok) => tok.node == check,
            None => false,
        }
    }

    fn current_is(&self, check: TokenKind) -> bool {
        match self.current {
            Some(tok) => tok.node == check,
            None => false,
        }
    }

    pub fn parse_program(&mut self) -> ParseResult<Program> {
        let mut program = Vec::new();
        while !self.eof {
            program.push(self.parse_statement()?)
        }
        Ok(program)
    }

    pub fn parse_statement(&mut self) -> ParseResult<Statement> {
        let start = self.span();
        let stmt = match self.current.unwrap().node {
            TokenKind::Let => self
                .parse_declaration_statement(DeclarationKind::Let)
                .map(StatementKind::Declaration)
                .map(|s| {
                    Spanned::builder()
                        .span((start.0, self.span().1))
                        .node(s)
                        .build()
                }),
            TokenKind::Import => self
                .parse_import_statement()
                .map(StatementKind::Import)
                .map(|s| {
                    Spanned::builder()
                        .span((start.0, self.span().1))
                        .node(s)
                        .build()
                }),
            TokenKind::Func => self
                .parse_function_statement()
                .map(StatementKind::Function)
                .map(|f| {
                    Spanned::builder()
                        .span((start.0, self.span().1))
                        .node(f)
                        .build()
                }),
            _ => self
                .parse_expression()
                .map(StatementKind::Expression)
                .map(|e| {
                    Spanned::builder()
                        .span((start.0, self.span().1))
                        .node(e)
                        .build()
                }),
        };

        while self.current_is(TokenKind::Newline) {
            self.advance();
        }

        stmt
    }

    fn parse_function_statement(&mut self) -> ParseResult<FunctionStatement> {
        self.advance();
        let ident = self.parse_identifier()?;
        self.expect(TokenKind::LeftParen);
    }

    fn parse_identifier(&mut self) -> ParseResult<Identifier> {
        if let Some(Spanned {
            span,
            node: TokenKind::Identifier(name),
        }) = self.current
        {
            self.advance();
            Ok(Spanned { span, node: name })
        } else {
            Err(ParseError::ExpectedFound(
                TokenKind::Identifier(String::new()),
                self.current.unwrap(),
            ))
        }
    }

    fn parse_expression(&mut self) -> ParseResult<Expression> {
        let start = self.span();
        let kind = match self.current.unwrap().node {
            TokenKind::Int(n) => ExpressionKind::Integer(n),
            TokenKind::Bool(b) => ExpressionKind::Boolean(b),
            _ => todo!("Support {:?}", self.current.unwrap()),
        };
        let end = self.span();
        self.advance();

        Ok(Spanned::builder().span((start.0, end.1)).node(kind).build())
    }

    fn parse_import_statement(&mut self) -> ParseResult<ImportStatement> {
        self.advance(); // Consume Import token

        // If the current token kind isn't a
        if let Some(Spanned {
            node: TokenKind::String(path),
            ..
        }) = self.current
        {
            self.advance();
            if self.current_is(TokenKind::As) {
                self.advance();
                if let Some(Spanned {
                    node: TokenKind::Identifier(alias),
                    span,
                }) = self.current
                {
                    self.advance();
                    return Ok(ImportStatement {
                        path: path.clone(),
                        alias: Some(Spanned::builder().span(span).node(alias.clone()).build()),
                    });
                } else {
                    return Err(ParseError::ImportDanglingAs);
                }
            }
            Ok(ImportStatement {
                path: path.clone(),
                alias: None,
            })
        } else {
            Err(ParseError::UnexpectedEndOfFile)
        }
    }

    fn parse_declaration_statement(
        &mut self,
        kind: DeclarationKind,
    ) -> ParseResult<DeclarationStatement> {
        todo!("implement declaration statement");
    }

    fn expect(&mut self, kind: TokenKind) -> ParseResult<()> {
        if self.current_is(kind) {
            self.advance();
            Ok(())
        } else {
            match self.current {
                Some(t) => Err(ParseError::ExpectedFound(kind, t)),
                None => Err(ParseError::UnexpectedEndOfFile),
            }
        }
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
