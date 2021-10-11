#![allow(dead_code)]
use serde::{Deserialize, Serialize};
use std::rc::Rc;
use typed_builder::TypedBuilder;

use crate::lex::{Operator, TokenKind};

/// A generic type for name resolution.
#[derive(Clone, Copy, PartialEq, Debug, Serialize, Deserialize)]
pub struct Reference<T, R> {
    raw: T,
    resolved: Option<R>,
}

impl<T, R> Reference<T, R> {
    pub fn new(raw: T) -> Self {
        Reference {
            raw,
            resolved: None,
        }
    }

    pub fn resolve(&mut self, resolved: R) {
        self.resolved = Some(resolved)
    }

    pub fn resolved(&self) -> bool {
        self.resolved.is_some()
    }
}

type Position = usize;

/// A pair of [`Position`]s, used by basically every AST node
pub type Span = (Position, Position);

#[derive(Debug, Clone, Default, Copy, TypedBuilder, PartialEq, Serialize, Deserialize)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn map<N, F>(self, func: F) -> Spanned<N>
    where
        F: Fn(T) -> N,
    {
        Spanned {
            node: func(self.node),
            span: self.span,
        }
    }
}

/// A module/program. Type signature subject to change
pub type Program = Vec<Statement>;

pub type Statement = Spanned<StatementKind>;
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum StatementKind {
    Import(ImportStatement),
    Function(FunctionStatement),
    Declaration(DeclarationStatement),
    Assign(AssignStatement),
    Return(Expression),
    Expression(Expression),
    For(ForStatement),
    Block(Vec<Statement>),
    TypeAlias(TypeAliasStatement),
}

impl Default for StatementKind {
    fn default() -> Self {
        Self::Expression(Expression::default())
    }
}

#[derive(Debug, Clone, Default, TypedBuilder, Serialize, Deserialize)]
pub struct ImportStatement {
    pub path: ModulePath,
    pub alias: Option<Identifier>,
}

impl From<ImportStatement> for StatementKind {
    fn from(f: ImportStatement) -> Self {
        StatementKind::Import(f)
    }
}

#[derive(Debug, Clone, Default, TypedBuilder, Serialize, Deserialize)]
pub struct ForStatement {
    pub(crate) name: Identifier,
    /// The collection that is being iterated through
    pub(crate) target: Box<Expression>,
    pub(crate) body: Vec<Statement>,
}

impl From<ForStatement> for StatementKind {
    fn from(f: ForStatement) -> Self {
        StatementKind::For(f)
    }
}

#[derive(Debug, Clone, Default, TypedBuilder, Serialize, Deserialize)]
pub struct FunctionStatement {
    body: Vec<Statement>,
    params: Vec<(Identifier, Type)>,
    return_type: Type,
    name: Identifier,
}

impl From<FunctionStatement> for StatementKind {
    fn from(f: FunctionStatement) -> Self {
        StatementKind::Function(f)
    }
}

#[derive(Debug, Clone, Default, TypedBuilder, Serialize, Deserialize)]
pub struct DeclarationStatement {
    kind: DeclarationKind,
    name: Identifier,
    typ: Option<Type>,
    value: Option<Box<Expression>>,
}

impl From<DeclarationStatement> for StatementKind {
    fn from(d: DeclarationStatement) -> Self {
        StatementKind::Declaration(d)
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum DeclarationKind {
    Let,
    Const,
}

impl Default for DeclarationKind {
    fn default() -> Self {
        DeclarationKind::Let
    }
}

#[derive(Debug, Clone, TypedBuilder, Serialize, Deserialize)]
pub struct AssignStatement {
    pub(crate) name: Identifier,
    pub(crate) value: Box<Expression>,
}

impl From<AssignStatement> for StatementKind {
    fn from(d: AssignStatement) -> Self {
        StatementKind::Assign(d)
    }
}

pub type Expression = Spanned<ExpressionKind>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BinOp {
    And,          // and
    Or,           // or
    Plus,         // +
    Minus,        // -
    Asterisk,     // *
    Slash,        // /
    Equal,        // ==
    NotEqual,     // !=
    Greater,      // >
    GreaterEqual, // >=
    Less,         // <
    LessEqual,    // <=

    PlusEqual,
    MinusEqual,
    AsteriskEqual,
    SlashEqual,
    Assign,

    Dot,
}

impl From<TokenKind> for Option<BinOp> {
    fn from(t: TokenKind) -> Self {
        match t {
            TokenKind::And => Some(BinOp::And),
            TokenKind::Or => Some(BinOp::Or),
            TokenKind::Dot => Some(BinOp::Dot),
            TokenKind::Assign => Some(BinOp::Assign),
            TokenKind::Operator(op) => Some(match op {
                Operator::Plus => BinOp::Plus,
                Operator::Minus => BinOp::Minus,
                Operator::Asterisk => BinOp::Asterisk,
                Operator::Slash => BinOp::Slash,

                Operator::PlusEqual => BinOp::PlusEqual,
                Operator::MinusEqual => BinOp::MinusEqual,
                Operator::AsteriskEqual => BinOp::AsteriskEqual,
                Operator::SlashEqual => BinOp::SlashEqual,

                Operator::Equal => BinOp::Equal,
                Operator::NotEqual => BinOp::NotEqual,
                Operator::Greater => BinOp::Greater,
                Operator::GreaterEqual => BinOp::GreaterEqual,
                Operator::Less => BinOp::Less,
                Operator::LessEqual => BinOp::LessEqual,
            }),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum UnaryOp {
    Not, // not
    Deref,
    Try,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExpressionKind {
    // Member(Box<Expression>, Identifier),
    Identifier(Identifier),
    Call {
        callee: Box<Expression>,
        args: Vec<Expression>,
    },
    If {
        cond: Box<Expression>,
        then: Vec<Statement>,
        otherwise: Option<Vec<Statement>>,
    },
    Integer(isize),
    Boolean(bool),
    Character(char),
    String(String),
    // TODO: Convert this into a struct instead of being inline
    Switch {
        cases: Vec<(Pattern, Statement)>,
        target: Box<Expression>,
    },
    BinOp(Box<Expression>, BinOp, Box<Expression>),
    UnaryOp(UnaryOp, Box<Expression>),
}

impl Default for ExpressionKind {
    fn default() -> Self {
        ExpressionKind::Boolean(true)
    }
}

pub type Pattern = Spanned<PatternKind>;
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PatternKind {
    Char(char),
    Integer(isize),
    Boolean(bool),
    String(String),
}

/// An identifier with an explicitly provided type.
/// A common example is for parameters of a function
#[derive(Debug, Clone, Default, TypedBuilder, Serialize, Deserialize)]
pub struct TypedIdentifier {
    /// The name of the value
    #[builder(default)]
    name: Identifier,
    /// The type of the value
    typ: Type,
}

#[derive(Debug, Clone, TypedBuilder, Serialize, Deserialize)]
pub struct TypeAliasStatement {
    name: Identifier,
    typ: Type,
}

pub type Type = Spanned<TypeKind>;
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeKind {
    Array {
        inner: Box<Type>,
        size: Option<usize>,
    },
    Int,
    Bool,
    Void,
    String,
    Char,
    Struct {
        fields: Vec<(Identifier, Type)>,
    },
    Function {
        params: Vec<(Identifier, Type)>,
        return_type: Box<Type>,
    },
    Nullable(Box<Type>),
    Fallable(Box<Type>),
    Pointer(Box<Type>),
    Custom(Reference<Identifier, Rc<Type>>),
}

impl Default for TypeKind {
    fn default() -> Self {
        TypeKind::Void
    }
}

pub type Identifier = Spanned<String>;
pub type ModulePath = Vec<Identifier>;

#[derive(Deserialize, Serialize)]
pub struct A(Rc<()>);
