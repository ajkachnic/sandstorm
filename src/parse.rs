use crate::ast::{
    AssignStatement, BinOp, DeclarationKind, DeclarationStatement, Expression, ExpressionKind,
    ForStatement, FunctionStatement, Identifier, ImportStatement, ModulePath, Pattern, PatternKind,
    Program, Reference, Span, Spanned, Statement, StatementKind, Type, TypeAliasStatement,
    TypeKind, UnaryOp,
};
use crate::cache::FileCache;
use crate::lex::{Operator, SpannedToken, TokenKind};
use crate::source::Source;

use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Assignment,
    Logical,        // and, or
    Equality,       // ==, !=
    Comparision,    // <, <=, >, >=
    Additive,       // +, -
    Multiplicative, // *, /
    Prefix,
    CallMember, // foobar(baz), foo.bar
}

impl From<TokenKind> for Precedence {
    fn from(t: TokenKind) -> Self {
        match t {
            TokenKind::Dot => Precedence::CallMember,
            TokenKind::LeftParen => Precedence::CallMember,
            // TODO: Make assignment right associative
            TokenKind::Assign => Precedence::Assignment,

            TokenKind::And => Precedence::Logical,
            TokenKind::Or => Precedence::Logical,

            TokenKind::Operator(op) => match op {
                Operator::Asterisk => Precedence::Multiplicative,
                Operator::Slash => Precedence::Multiplicative,
                Operator::Plus => Precedence::Additive,
                Operator::Minus => Precedence::Additive,

                Operator::AsteriskEqual => Precedence::Assignment,
                Operator::SlashEqual => Precedence::Assignment,
                Operator::PlusEqual => Precedence::Assignment,
                Operator::MinusEqual => Precedence::Assignment,

                Operator::Greater => Precedence::Comparision,
                Operator::GreaterEqual => Precedence::Comparision,
                Operator::Less => Precedence::Comparision,
                Operator::LessEqual => Precedence::Comparision,
                Operator::Equal => Precedence::Equality,
                Operator::NotEqual => Precedence::Equality,
                _ => Precedence::Lowest,
            },
            _ => Precedence::Lowest,
        }
    }
}

#[derive(Debug, Error, Diagnostic)]
pub enum ParseError {
    #[error("Unexpected end of file")]
    #[diagnostic(
        code("sandstorm.parse.unexpected_eof"),
        help("Write better code next time")
    )]
    UnexpectedEndOfFile,

    #[error("Expected token kind {expected:?}, found token {found:?}")]
    #[diagnostic(
        code("sandstorm.parse.expected_found"),
        help("Write better code next time")
    )]
    ExpectedFound {
        expected: TokenKind,
        found: TokenKind,
        #[source_code]
        src: std::sync::Arc<String>,
        #[label("This bit here")]
        span: Span,
    },

    #[error("Token cannot be used as type")]
    #[diagnostic(
        code("sandstorm.parse.invalid_type"),
        help("Cross your I's and dot your T's")
    )]
    InvalidType {
        #[label("This bit here")]
        span: Span,
        #[source_code]
        src: std::sync::Arc<String>,
    },

    #[error("Token cannot be transformed into a character literal")]
    #[diagnostic(
        code("sandstorm.parse.invalid_char"),
        help("This is likely a compiler bug. Consider opening an issue")
    )]
    InvalidChar {
        #[label("This bit here")]
        span: Span,
        #[source_code]
        src: std::sync::Arc<String>,
    },

    #[error("Token cannot be transformed into a numeric literal")]
    #[diagnostic(
        code("sandstorm.parse.invalid_number"),
        help("This is likely a compiler bug. Consider opening an issue")
    )]
    InvalidNumber {
        #[label("This bit here")]
        span: Span,
        #[source_code]
        src: std::sync::Arc<String>,
    },

    #[error("Invalid array size: {size}")]
    #[diagnostic(
        code("sandstorm.parse.invalid_array_size"),
        help("Make sure the array size is a positive number")
    )]
    InvalidArraySize {
        #[label("This bit here")]
        span: Span,
        #[source_code]
        src: std::sync::Arc<String>,

        size: isize,
    },
    #[error("While parsing an expression, we came upon an unsupported prefix: {tok:?}")]
    #[diagnostic(
        code("sandstorm.parse.unsupported_prefix"),
        help("Check the surrounding area in your code."),
        help("Consider opening an issue on how we can improve this error message")
    )]
    UnsupportedPrefix {
        #[label("This bit here")]
        span: Span,
        #[source_code]
        src: std::sync::Arc<String>,

        tok: TokenKind,
    },
}

pub type ParseResult<T> = Result<T, ParseError>;
#[derive(Debug)]
pub struct Parser<'a> {
    pos: usize,
    tokens: Vec<SpannedToken>,
    current: Option<SpannedToken>,
    peek: Option<SpannedToken>,
    cache: &'a mut FileCache,
    source: Source,
    eof: bool,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<SpannedToken>, source: Source, cache: &'a mut FileCache) -> Self {
        let mut parser = Self {
            pos: 1,
            eof: false,
            tokens,
            cache,
            current: None,
            peek: None,
            source,
        };
        parser.current = parser.tokens.get(0).copied();
        parser.peek = parser.tokens.get(1).copied();
        parser
    }

    fn span(&self) -> Span {
        match self.current {
            Some(s) => s.span,
            None => (0, 0),
        }
    }

    fn advance(&mut self) {
        self._advance();
    }

    fn _advance(&mut self) {
        self.pos += 1;
        self.current = self.peek;
        self.peek = self.tokens.get(self.pos).copied();
        if self.pos >= self.tokens.len() {
            self.eof = true;
        }
    }

    fn skip_semi(&mut self) {
        while self.current_is(TokenKind::Semicolon) {
            self.advance();
        }
        println!("skipped semis, next is {:?}", self.current);
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

    fn precedence(&self) -> Precedence {
        match self.current {
            Some(t) => Precedence::from(t.node),
            None => Precedence::Lowest,
        }
    }

    fn peek_precedence(&self) -> Precedence {
        match self.peek {
            Some(t) => Precedence::from(t.node),
            None => Precedence::Lowest,
        }
    }

    fn current_slice(&mut self) -> String {
        self.source.slice(self.cache, self.current.unwrap().span)
    }

    fn peek_slice(&mut self) -> String {
        self.source.slice(self.cache, self.current.unwrap().span)
    }

    fn convert_statement<F, S>(value: ParseResult<F>, kind: S, span: Span) -> ParseResult<Statement>
    where
        S: Fn(F) -> StatementKind,
    {
        value
            .map(kind)
            .map(|s| Spanned::builder().span(span).node(s).build())
    }

    fn split(&self, start: Span) -> Span {
        (start.0, self.span().1)
    }
    pub fn parse_program(&mut self) -> ParseResult<Program> {
        let mut program = Vec::new();
        while !self.eof {
            program.push(self.parse_statement()?);
            self.skip_semi();
        }
        Ok(program)
    }

    pub fn parse_statement(&mut self) -> ParseResult<Statement> {
        println!("parsing statement {:?}", self.current);
        let start = self.span();
        let stmt = match self.current.unwrap().node {
            TokenKind::Let => Self::convert_statement(
                self.parse_declaration_statement(DeclarationKind::Let),
                StatementKind::Declaration,
                self.split(start),
            ),
            TokenKind::Return => self.parse_return_statement(),
            TokenKind::Import => Self::convert_statement(
                self.parse_import_statement(),
                StatementKind::Import,
                self.split(start),
            ),
            TokenKind::Func => Self::convert_statement(
                self.parse_function_statement(),
                StatementKind::Function,
                self.split(start),
            ),
            TokenKind::For => Self::convert_statement(
                self.parse_for_statement(),
                StatementKind::For,
                self.split(start),
            ),
            TokenKind::Type => Self::convert_statement(
                self.parse_type_alias_statement(),
                StatementKind::TypeAlias,
                self.split(start),
            ),
            _ => Self::convert_statement(
                self.parse_expression(),
                StatementKind::Expression,
                self.split(start),
            ),
        };

        stmt
    }

    fn parse_assign_statement(&mut self) -> ParseResult<AssignStatement> {
        let name = self.parse_identifier()?;
        self.expect(TokenKind::Operator(Operator::Equal))?;
        let value = self.parse_expression()?;

        Ok(AssignStatement::builder()
            .name(name)
            .value(Box::new(value))
            .build())
    }

    fn parse_block_statement(&mut self) -> ParseResult<Vec<Statement>> {
        self.expect(TokenKind::LeftCurlyBrace)?;
        let mut body = Vec::new();

        while !self.current_is(TokenKind::RightCurlyBrace) && self.current != None {
            body.push(self.parse_statement()?)
        }

        self.expect(TokenKind::RightCurlyBrace)?;

        self.skip_semi();

        Ok(body)
    }

    fn parse_for_statement(&mut self) -> ParseResult<ForStatement> {
        self.expect(TokenKind::For)?;
        let name = self.parse_identifier()?;
        self.expect(TokenKind::In)?;
        let target = self.parse_expression()?;

        let body = self.parse_block_statement()?;

        Ok(ForStatement::builder()
            .name(name)
            .target(Box::new(target))
            .body(body)
            .build())
    }

    fn parse_function_statement(&mut self) -> ParseResult<FunctionStatement> {
        self.advance();
        let name = self.parse_identifier()?;
        self.expect(TokenKind::LeftParen)?;

        let mut params = Vec::new();

        if self.current_is(TokenKind::RightParen) {
            self.advance();
        } else {
            loop {
                let ident = self.parse_identifier()?;
                let typ = self.parse_type()?;

                params.push((ident, typ));

                if self.current_is(TokenKind::Comma) {
                    self.advance()
                } else {
                    break;
                }
            }

            self.expect(TokenKind::RightParen)?;
        }

        let return_type = match self.parse_type() {
            Ok(t) => t,
            Err(_) => Spanned::builder().span((0, 0)).node(TypeKind::Void).build(),
        };

        let body = self.parse_block_statement()?;

        Ok(FunctionStatement::builder()
            .return_type(return_type)
            .body(body)
            .params(params)
            .name(name)
            .build())
    }

    fn parse_import_statement(&mut self) -> ParseResult<ImportStatement> {
        self.advance(); // Consume Import token

        let path = self.parse_module_path()?;
        if self.current_is(TokenKind::As) {
            self.advance();
            let ident = self.parse_identifier()?;
            return Ok(ImportStatement {
                path,
                alias: Some(ident),
            });
        }

        Ok(ImportStatement { path, alias: None })
    }

    fn parse_type_alias_statement(&mut self) -> ParseResult<TypeAliasStatement> {
        self.advance();

        let name = self.parse_identifier()?;
        let typ = self.parse_type()?;

        Ok(TypeAliasStatement::builder().name(name).typ(typ).build())
    }

    fn parse_return_statement(&mut self) -> ParseResult<Statement> {
        let start = self.span();
        self.expect(TokenKind::Return)?;
        let e = self.parse_expression()?;

        Ok(Statement::builder()
            .node(StatementKind::Return(e))
            .span((start.0, self.span().1))
            .build())
    }

    fn parse_declaration_statement(
        &mut self,
        kind: DeclarationKind,
    ) -> ParseResult<DeclarationStatement> {
        println!("parsing decl");
        self.advance();
        let name = self.parse_identifier()?;
        let mut typ = None;

        if self.current_is(TokenKind::Colon) {
            self.advance();
            typ = Some(self.parse_type()?);
        }

        let mut value = None;
        if self.current_is(TokenKind::Assign) {
            self.advance();
            value = Some(Box::new(self.parse_expression()?));
        }

        Ok(DeclarationStatement::builder()
            .kind(kind)
            .name(name)
            .value(value)
            .typ(typ)
            .build())
    }

    fn parse_expression(&mut self) -> ParseResult<Expression> {
        self._parse_expression(Precedence::Lowest)
    }

    fn _parse_expression(&mut self, pred: Precedence) -> ParseResult<Expression> {
        println!("parsing expression {:?}", self.current);
        let mut left = self.parse_prefix()?;

        while pred < self.precedence() {
            left = self.parse_infix_expression(left)?;
        }
        self.skip_semi();

        Ok(left)
    }

    fn parse_prefix(&mut self) -> ParseResult<Expression> {
        let start = self.span();
        let kind = match self.current.unwrap().node {
            TokenKind::NumberLiteral => self.parse_number()?,
            TokenKind::StringLiteral => ExpressionKind::String(self.parse_string()?.node),
            TokenKind::True => {
                self.advance();
                ExpressionKind::Boolean(true)
            }
            TokenKind::False => {
                self.advance();
                ExpressionKind::Boolean(false)
            }
            TokenKind::Identifier => ExpressionKind::Identifier(self.parse_identifier()?),
            TokenKind::LeftParen => self.parse_grouped_expression()?,
            TokenKind::Switch => self.parse_switch()?,
            TokenKind::If => self.parse_if_expression()?,
            TokenKind::Not => {
                self.advance();
                ExpressionKind::UnaryOp(
                    UnaryOp::Not,
                    Box::new(self._parse_expression(Precedence::Prefix)?),
                )
            }
            TokenKind::Operator(Operator::Asterisk) => {
                self.advance();
                ExpressionKind::UnaryOp(
                    UnaryOp::Deref,
                    Box::new(self._parse_expression(Precedence::Prefix)?),
                )
            }
            TokenKind::Try => {
                self.advance();
                ExpressionKind::UnaryOp(
                    UnaryOp::Try,
                    Box::new(self._parse_expression(Precedence::Prefix)?),
                )
            }
            kind => {
                let span = self.current.unwrap().span;
                return Err(ParseError::UnsupportedPrefix {
                    span: (span.0, span.1 - span.0),
                    src: self.source.get(self.cache),
                    tok: kind,
                });
            }
        };
        let end = self.span();

        Ok(Spanned::builder().span((start.0, end.1)).node(kind).build())
    }

    fn parse_number(&mut self) -> ParseResult<ExpressionKind> {
        let num = self.get_number()?;
        self.advance();
        Ok(ExpressionKind::Integer(num))
    }

    fn parse_grouped_expression(&mut self) -> ParseResult<ExpressionKind> {
        self.advance();

        let exp = self.parse_expression()?;

        self.expect(TokenKind::RightParen)?;

        Ok(exp.node)
    }

    fn parse_infix_expression(&mut self, left: Expression) -> ParseResult<Expression> {
        match self.current.unwrap().node {
            TokenKind::LeftParen => self.parse_call_expression(left),
            _ => self.parse_binary_expression(left),
        }
    }

    fn parse_call_expression(&mut self, func: Expression) -> ParseResult<Expression> {
        let start = func.span;
        self.advance();
        let mut args = Vec::new();

        if self.current_is(TokenKind::RightParen) {
            self.advance();
        } else {
            args.push(self.parse_expression()?);
            while self.current_is(TokenKind::Comma) {
                self.advance();
                args.push(self.parse_expression()?);
            }
            self.expect(TokenKind::RightParen)?;
        }

        let end = self.span();
        let call = ExpressionKind::Call {
            callee: Box::new(func),
            args,
        };

        Ok(Spanned::builder().span((start.0, end.1)).node(call).build())
    }

    fn parse_binary_expression(&mut self, left: Expression) -> ParseResult<Expression> {
        let start = left.span;
        let precedence = self.precedence();
        let op: Option<BinOp> = self.current.unwrap().node.into();
        let op = op.unwrap();
        self.advance();
        let right = self._parse_expression(precedence)?;

        let kind = ExpressionKind::BinOp(Box::new(left), op, Box::new(right));

        let end = self.span();

        Ok(Spanned::builder().span((start.0, end.1)).node(kind).build())
    }

    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        let start = self.span();
        let kind = match self.current.unwrap().node {
            TokenKind::NumberLiteral => PatternKind::Integer(self.get_number()?),
            TokenKind::True => PatternKind::Boolean(true),
            TokenKind::False => PatternKind::Boolean(false),
            TokenKind::CharLiteral => PatternKind::Char(self.get_char()?),
            _ => todo!("Support {:?}", self.current.unwrap()),
        };
        let end = self.span();
        self.advance();
        Ok(Spanned::builder().span((start.0, end.1)).node(kind).build())
    }

    fn parse_switch(&mut self) -> ParseResult<ExpressionKind> {
        self.expect(TokenKind::Switch)?;
        let target = self.parse_expression()?;

        self.expect(TokenKind::LeftCurlyBrace)?;

        let mut cases = Vec::new();

        while self.current.map(|n| n.node) != Some(TokenKind::RightCurlyBrace) {
            let pattern = self.parse_pattern()?;
            self.expect(TokenKind::DoubleArrow)?;
            let start = self.span();

            // TODO: Support block statements
            let stmt = Self::convert_statement(
                self.parse_expression(),
                StatementKind::Expression,
                self.split(start),
            )?;

            cases.push((pattern, stmt));

            if self.current_is(TokenKind::Semicolon) {
                self.advance();
            }
        }
        self.expect(TokenKind::RightCurlyBrace)?;
        self.skip_semi();

        Ok(ExpressionKind::Switch {
            cases,
            target: Box::new(target),
        })
    }

    fn parse_if_expression(&mut self) -> ParseResult<ExpressionKind> {
        self.expect(TokenKind::If)?;
        let cond = self.parse_expression()?;

        println!("cond - {:?}", cond);

        let then = self.parse_block_statement()?;

        self.skip_semi();

        let otherwise = if self.current_is(TokenKind::Else) {
            self.advance();
            if self.current_is(TokenKind::If) {
                let expr = self.parse_expression()?;
                Some(vec![Spanned::builder()
                    .span(expr.span)
                    .node(StatementKind::Expression(expr))
                    .build()])
            } else {
                Some(self.parse_block_statement()?)
            }
        } else {
            None
        };

        Ok(ExpressionKind::If {
            cond: Box::new(cond),
            then,
            otherwise,
        })
    }

    fn parse_type(&mut self) -> ParseResult<Type> {
        if let Some(tok) = self.current {
            let span = tok.span;
            // TODO: Expand upon this and add parsing for other types
            let kind = match tok.node {
                TokenKind::Void => {
                    self.advance();
                    TypeKind::Void
                }
                TokenKind::Bool => {
                    self.advance();
                    TypeKind::Bool
                }
                TokenKind::Int => {
                    self.advance();
                    TypeKind::Int
                }
                TokenKind::String => {
                    self.advance();
                    TypeKind::String
                }

                TokenKind::Question => {
                    self.advance();
                    TypeKind::Nullable(Box::new(self.parse_type()?))
                }
                TokenKind::Bang => {
                    self.advance();
                    TypeKind::Fallable(Box::new(self.parse_type()?))
                }
                TokenKind::Operator(Operator::Asterisk) => {
                    self.advance();
                    TypeKind::Pointer(Box::new(self.parse_type()?))
                }
                TokenKind::LeftSquareBracket => {
                    self.advance();
                    let size = if self.current_is(TokenKind::NumberLiteral) {
                        let num = self.get_number()?;
                        if num < 0 {
                            let s = self.current.unwrap().span;
                            return Err(ParseError::InvalidArraySize {
                                span: (s.0, s.1 - s.0),
                                src: self.source.get(self.cache),
                                size: num,
                            });
                        }
                        self.advance();
                        Some(num as usize)
                    } else {
                        None
                    };
                    self.expect(TokenKind::RightSquareBracket)?;
                    TypeKind::Array {
                        inner: Box::new(self.parse_type()?),
                        size,
                    }
                }

                TokenKind::Identifier => TypeKind::Custom(Reference::new(self.parse_identifier()?)),

                _ => {
                    return Err(ParseError::InvalidType {
                        span: (span.0, span.1 - span.0),
                        src: self.source.get(self.cache),
                    })
                }
            };

            return Ok(Spanned::builder().node(kind).span(tok.span).build());
        }
        Err(ParseError::UnexpectedEndOfFile)
    }

    fn parse_identifier(&mut self) -> ParseResult<Identifier> {
        if let Some(Spanned {
            span,
            node: TokenKind::Identifier,
        }) = self.current
        {
            let name = self.current_slice();
            self.advance();
            Ok(Spanned { span, node: name })
        } else {
            Err(self.expect_error(TokenKind::Identifier))
        }
    }

    fn parse_string(&mut self) -> ParseResult<Spanned<String>> {
        if let Some(Spanned {
            span,
            node: TokenKind::StringLiteral,
        }) = self.current
        {
            let str = self.source.slice(self.cache, (span.0 + 1, span.1 - 1));
            self.advance();
            Ok(Spanned { span, node: str })
        } else {
            Err(self.expect_error(TokenKind::StringLiteral))
        }
    }

    fn parse_module_path(&mut self) -> ParseResult<ModulePath> {
        let mut path = vec![self.parse_identifier()?];

        while self.current_is(TokenKind::Dot) {
            self._advance();
            path.push(self.parse_identifier()?);
        }

        Ok(path)
    }

    /*******************
     ** GET FUNCTIONS **
     *******************/

    // get-functions parse the current span to retreive information for tokens.
    // They dont't check the token, and thus can easily fail if ran on the wrong token
    #[inline]
    fn get_char(&mut self) -> ParseResult<char> {
        let slice = self.current_slice();
        if slice.len() == 3 {
            Ok(slice.chars().nth(1).unwrap())
        } else if slice.len() == 2 {
            Ok(0 as char)
        } else {
            let span = self.span();
            Err(ParseError::InvalidChar {
                span: (span.0, span.1 - span.0),
                src: self.source.get(self.cache),
            })
        }
    }

    #[inline]
    fn get_number(&mut self) -> ParseResult<isize> {
        let span = self.span();
        match self.current_slice().parse() {
            Ok(num) => Ok(num),
            Err(_) => Err(ParseError::InvalidNumber {
                span: (span.0, span.1 - span.0),
                src: self.source.get(self.cache),
            }),
        }
    }
    #[inline]
    fn get_ident(&mut self) -> ParseResult<Identifier> {
        let span = self.span();
        let slice = self.current_slice();

        Ok(Spanned::builder().span(span).node(slice).build())
    }

    fn expect(&mut self, kind: TokenKind) -> ParseResult<()> {
        if self.current_is(kind) {
            self.advance();
            Ok(())
        } else {
            Err(self.expect_error(kind))
        }
    }

    fn expect_error(&mut self, kind: TokenKind) -> ParseError {
        match self.current {
            Some(t) => ParseError::ExpectedFound {
                expected: kind,
                found: t.node,
                span: (t.span.0, t.span.1 - t.span.0),
                src: self.source.get(self.cache),
            },
            None => ParseError::UnexpectedEndOfFile,
        }
    }
}
