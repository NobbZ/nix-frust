// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use std::rc::Rc;

use codemap::{File, Span};
use rnix::ast;
use rowan::ast::AstNode;

use crate::compiler::chunk::CodeIdx;
use crate::values::Lambda;

/// Helper struct to carry information required for making a span, but
/// without actually performing the (expensive) span lookup.
///
/// This is used for tracking spans across thunk boundaries, as they
/// are frequently instantiated but spans are only used in error or
/// warning cases.
#[derive(Clone, Debug)]
pub enum LightSpan {
    /// The span has already been computed and can just be used right
    /// away.
    Actual { span: Span },

    /// The span needs to be computed from the provided data, but only
    /// when it is required.
    Delayed { lambda: Rc<Lambda>, offset: CodeIdx },
}

impl LightSpan {
    pub fn new_delayed(lambda: Rc<Lambda>, offset: CodeIdx) -> Self {
        Self::Delayed { lambda, offset }
    }

    pub fn new_actual(span: Span) -> Self {
        Self::Actual { span }
    }

    pub fn span(&self) -> Span {
        match self {
            LightSpan::Actual { span } => *span,
            LightSpan::Delayed { lambda, offset } => lambda.chunk.get_span(*offset),
        }
    }
}

impl From<Span> for LightSpan {
    fn from(span: Span) -> Self {
        LightSpan::Actual { span }
    }
}

pub trait ToSpan {
    fn to_span(&self, file: &File) -> Span;
}

impl<T> ToSpan for &T
where
    T: ToSpan,
{
    fn to_span(&self, file: &File) -> Span {
        (*self).to_span(file)
    }
}

impl ToSpan for Span {
    fn to_span(&self, _: &File) -> Span {
        *self
    }
}

impl ToSpan for rnix::TextRange {
    fn to_span(&self, file: &File) -> Span {
        file.span
            .subspan(u32::from(self.start()) as u64, u32::from(self.end()) as u64)
    }
}

impl ToSpan for rnix::SyntaxNode {
    fn to_span(&self, file: &File) -> Span {
        self.text_range().to_span(file)
    }
}

/// Generates a `ToSpan` implementation for a type implementing
/// `rowan::AstNode`. This is impossible to do as a blanket
/// implementation because `rustc` forbids these implementations for
/// traits from third-party crates due to a belief that semantic
/// versioning truly could work (it doesn't).
macro_rules! expr_to_span {
    ( $type:path ) => {
        impl ToSpan for $type {
            fn to_span(&self, file: &File) -> Span {
                self.syntax().to_span(file)
            }
        }
    };
}

expr_to_span!(ast::Expr);
expr_to_span!(ast::Interpol);
expr_to_span!(ast::Literal);
expr_to_span!(ast::Str);
