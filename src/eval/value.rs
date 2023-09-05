// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use std::{collections::HashMap, fmt::Debug, rc::Rc};

use eyre::Result;
use nil_syntax::ast::{Expr, Lambda};

use crate::eval::context::Context;

#[derive(Clone)]
pub enum Value {
    AttrSet(HashMap<String, Value>),
    Bool(bool),
    Float(f64),
    Integer(i64),
    List(Vec<Value>),
    Path(String),
    String(String),
    Lambda(Rc<dyn Fn(Value) -> Result<Value>>, Lambda),
    Thunk(Expr, Context),
}

impl PartialEq<Value> for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::AttrSet(lhs), Value::AttrSet(rhs)) => lhs == rhs,
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Float(lhs), Value::Float(rhs)) => lhs == rhs,
            (Value::Integer(lhs), Value::Integer(rhs)) => lhs == rhs,
            (Value::List(lhs), Value::List(rhs)) => lhs == rhs,
            (Value::Path(lhs), Value::Path(rhs)) => lhs == rhs,
            (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
            (Value::Lambda(_, _), Value::Lambda(_, _)) => false,
            (Value::Thunk(_, _), Value::Thunk(_, _)) => false,
            _ => false,
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AttrSet(set) => f.debug_tuple("AttrSet").field(set).finish(),
            Self::Bool(bool) => f.debug_tuple("Bool").field(bool).finish(),
            Self::Float(n) => f.debug_tuple("Float").field(n).finish(),
            Self::Integer(i) => f.debug_tuple("Integer").field(i).finish(),
            Self::List(xs) => f.debug_tuple("List").field(xs).finish(),
            Self::Path(p) => f.debug_tuple("Path").field(p).finish(),
            Self::String(s) => f.debug_tuple("String").field(s).finish(),
            Self::Thunk(t, ctx) => f.debug_tuple("Thunk").field(t).field(ctx).finish(),
            Self::Lambda(_, l) => f
                .debug_tuple("Lambda")
                .field(&UnderScore {})
                .field(&LambdaDebug(l))
                .finish(),
        }
    }
}

struct UnderScore {}

impl Debug for UnderScore {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("_")
    }
}

struct LambdaDebug<'a>(&'a Lambda);

impl Debug for LambdaDebug<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let code = self
            .0
            .colon_token()
            .and_then(|t| t.parent())
            .map(|p| p.text())
            .ok_or(std::fmt::Error)?;

        f.write_fmt(format_args!("<{}>", code))
    }
}
