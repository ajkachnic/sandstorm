use crate::ast::{Expression, ExpressionKind, Statement, StatementKind, Type, TypeKind};
use std::collections::HashMap;
use std::rc::Rc;
use thiserror::Error;

struct TypeEnv {
    map: HashMap<String, Rc<TypeKind>>,
    parent: Option<Rc<TypeEnv>>,
}

impl TypeEnv {
    fn new() -> Self {
        TypeEnv {
            map: HashMap::new(),
            parent: None,
        }
    }

    fn set_type(&mut self, key: String, t: TypeKind) {
        self.map.insert(key, Rc::new(t));
    }

    fn get_type(&self, key: &str) -> Option<Rc<TypeKind>> {
        match self.map.get(key) {
            Some(t) => Some(t.clone()),
            None => {
                if let Some(parent) = &self.parent {
                    parent.get_type(key)
                    //return parent.get_type(key);
                } else {
                    None
                }
            }
        }
    }
}

#[derive(Error, Debug)]
pub enum TypeError {
    #[error("Expected type {expected:?}, found type {found:?}")]
    ExpectedFound {
        expected: Rc<TypeKind>,
        found: Rc<TypeKind>,
    },

    #[error("Could not find identifier {0} in current scope")]
    UnresolvedIdentifier(String),

    #[error(
        "Mismatched number of arguments passed to function. Received {args}, expected {params}"
    )]
    MismatchedArgsParams { args: usize, params: usize },

    #[error("Expected function, received {0:?} instead")]
    ExpectedFunction(TypeKind),

    #[error("Undefined variable {0}")]
    UndefinedVariable(String),
}

pub type TypeResult<T> = Result<T, TypeError>;

struct TypeChecker {
    env: TypeEnv,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            env: TypeEnv::new(),
        }
    }

    pub fn check_statement(&mut self, stmt: Statement) -> TypeResult<()> {
        match stmt.node {
            StatementKind::Assign(assign) => {
                let name_type = match self.env.get_type(&assign.name.node) {
                    Some(t) => t,
                    None => return Err(TypeError::UndefinedVariable(assign.name.node)),
                };
                let value_type = self.check_expression(*assign.value)?;
                if value_type != name_type {
                    return Err(TypeError::ExpectedFound {
                        expected: name_type,
                        found: value_type,
                    });
                }
                Ok(())
            }
            StatementKind::Expression(e) => {
                self.check_expression(e)?;
                Ok(())
            }
            _ => Ok(()),
        }
    }

    pub fn check_expression(&mut self, expr: Expression) -> TypeResult<Rc<TypeKind>> {
        match expr.node {
            ExpressionKind::Integer(_) => Ok(Rc::new(TypeKind::Int)),
            ExpressionKind::Boolean(_) => Ok(Rc::new(TypeKind::Bool)),
            ExpressionKind::Identifier(ident) => match self.env.get_type(&ident.node) {
                Some(t) => Ok(t),
                None => Err(TypeError::UnresolvedIdentifier(ident.node)),
            },
            ExpressionKind::Switch { .. } => todo!("implement switch checking"),
            ExpressionKind::BinOp { .. } => todo!("implement infix"),
            ExpressionKind::UnaryOp { .. } => todo!("implement unary"),
            ExpressionKind::Call { callee, args } => match self.check_expression(*callee)?.as_ref()
            {
                TypeKind::Function {
                    params,
                    return_type,
                } => {
                    if args.len() != params.len() {
                        return Err(TypeError::MismatchedArgsParams {
                            args: args.len(),
                            params: params.len(),
                        });
                    }
                    for (arg, param) in args.into_iter().zip(params) {
                        let arg_type = self.check_expression(arg)?;
                        if arg_type.as_ref() != &param.1.node {
                            return Err(TypeError::ExpectedFound {
                                expected: arg_type,
                                found: Rc::new(param.1.node.clone()),
                            });
                        }
                    }
                    Ok(Rc::new(return_type.node.clone()))
                }
                t => Err(TypeError::ExpectedFunction(t.clone())),
            },
        }
    }
}
