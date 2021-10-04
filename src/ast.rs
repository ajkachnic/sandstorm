#![allow(dead_code)]
use typed_builder::TypedBuilder;

/// A generic type for name resolution.
pub struct Reference<T, R> {
    raw: T,
    resolved: Option<R>,
}

type Position = usize;

/// A pair of [`Position`]s, used by basically every AST node
pub type Span = (Position, Position);

#[derive(Debug, Clone, Default, TypedBuilder, PartialEq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

pub type Statement = Spanned<StatementKind>;
#[derive(Debug, Clone)]
pub enum StatementKind {
    Import(ImportStatement),
    Function(FunctionStatement),
    Declaration(DeclarationStatement),
    Assign(AssignStatement),
    Return(Expression),
    Expression(Expression),
}

impl Default for StatementKind {
    fn default() -> Self {
        Self::Expression(Expression::default())
    }
}

#[derive(Debug, Clone, Default, TypedBuilder)]
pub struct ImportStatement {
    #[builder(default, setter(into))]
    path: String,
    alias: Option<Identifier>,
}

impl From<ImportStatement> for StatementKind {
    fn from(f: ImportStatement) -> Self {
        StatementKind::Import(f)
    }
}

#[derive(Debug, Clone, Default, TypedBuilder)]
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

#[derive(Debug, Clone, Default)]
pub struct DeclarationStatement {
    kind: DeclarationKind,
    name: Identifier,
    typ: Option<Type>,
    value: Box<Expression>,
}

impl From<DeclarationStatement> for StatementKind {
    fn from(d: DeclarationStatement) -> Self {
        StatementKind::Declaration(d)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum DeclarationKind {
    Let,
    Const,
}

impl Default for DeclarationKind {
    fn default() -> Self {
        DeclarationKind::Let
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    // Member(Box<Expression>, Identifier),
    Identifier(Identifier),
    Call {
        callee: Box<Expression>,
        args: Vec<Expression>,
    },
    Integer(usize),
    Boolean(bool),
}

impl Default for ExpressionKind {
    fn default() -> Self {
        ExpressionKind::Boolean(true)
    }
}

/// An identifier with an explicitly provided type.
/// A common example is for parameters of a function
#[derive(Debug, Clone, Default, TypedBuilder)]
pub struct TypedIdentifier {
    /// The name of the value
    #[builder(default)]
    name: Identifier,
    /// The type of the value
    typ: Type,
}

pub type Type = Spanned<TypeKind>;
#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Array(Box<Type>),
    Int,
    Bool,
    Void,
    Struct {
        fields: Vec<(String, Type)>,
    },
    Function {
        params: Vec<(Identifier, Type)>,
        return_type: Box<Type>,
    },
}

impl Default for TypeKind {
    fn default() -> Self {
        TypeKind::Void
    }
}

pub type Identifier = Spanned<String>;
