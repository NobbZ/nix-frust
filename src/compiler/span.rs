// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use codemap::{File, Span};
use rnix::ast::{self};
use rowan::ast::AstNode;

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
expr_to_span!(ast::Literal);
