// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use crate::compiler::opcode::OpCode;
use crate::compiler::span::ToSpan;
use crate::eval::value::Value;

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct ConstIdx(usize);

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct CodeIdx(usize);

/// Represents a source location from which one or more operations
/// were compiled.
///
/// The span itself is an index into a [codemap::Codemap], and the
/// structure tracks the number of operations that were yielded from
/// the same span.
///
/// At error reporting time, it becomes possible to either just fetch
/// the textual representation of that span from the codemap, or to
/// even re-parse the AST using rnix to create more semantically
/// interesting errors.
#[derive(Clone, Debug, PartialEq)]
struct SourceSpan {
    /// Span into the [codemap::Codemap].
    span: codemap::Span,

    /// Index of the first operation covered by this span.
    start: usize,
}

#[derive(Debug)]
pub(crate) struct Chunk {
    pub opcodes: Vec<OpCode>,
    pub constants: Vec<Value>,
    spans: Vec<SourceSpan>,
}

impl Chunk {
    pub(crate) fn new() -> Self {
        Self {
            opcodes: vec![],
            constants: vec![],
            spans: vec![],
        }
    }

    pub(crate) fn push_op<S: ToSpan>(&mut self, op: OpCode, node: S) -> CodeIdx {
        let idx = self.opcodes.len();
        self.opcodes.push(op);
        CodeIdx(idx)
    }

    pub(crate) fn push_const(&mut self, value: Value) -> ConstIdx {
        let idx = match self
            .constants
            .iter()
            .enumerate()
            .find(|(_, v)| **v == value)
        {
            Some((idx, _)) => idx,
            None => {
                self.constants.push(value);
                self.constants.len() - 1
            }
        };

        ConstIdx(idx)
    }

    /// Extend this chunk with the content of another, moving out of the other
    /// in the process.
    ///
    /// This is used by the compiler when it detects that it unnecessarily
    /// thunked a nested expression.
    pub fn extend(&mut self, other: Self) {
        // Some operations need to be modified in certain ways before being
        // valid as part of the new chunk.
        let const_count = self.constants.len();
        for (idx, op) in other.opcodes.iter().enumerate() {
            let span = other.get_span(CodeIdx(idx));
            match op {
                // As the constants shift, the index needs to be moved relatively.
                OpCode::Const(ConstIdx(idx)) => self.push_op(OpCode::Const(ConstIdx(idx + const_count)), span),

                // Other operations either operate on relative offsets, or no
                // offsets, and are safe to keep as-is.
                _ => self.push_op(*op, span),
            };
        }

        self.constants.extend(other.constants);
        self.spans.extend(other.spans);
    }

    /// Retrieve the [codemap::Span] from which the instruction at
    /// `offset` was compiled.
    pub fn get_span(&self, offset: CodeIdx) -> codemap::Span {
        let position = self
            .spans
            .binary_search_by(|span| span.start.cmp(&offset.0));

        let span = match position {
            Ok(index) => &self.spans[index],
            Err(index) => {
                if index == 0 {
                    &self.spans[0]
                } else {
                    &self.spans[index - 1]
                }
            }
        };

        span.span
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::{assert_eq, assert_ne};

    // TODO: make this a property test
    #[test]
    fn test_push_same_const() {
        let mut chunk = Chunk::new();

        let idx1 = chunk.push_const(Value::Integer(42));
        let idx2 = chunk.push_const(Value::Integer(42));

        assert_eq!(idx1, idx2);
    }

    #[test]
    fn test_push_diff_const() {
        let mut chunk = Chunk::new();

        let idx1 = chunk.push_const(Value::Integer(42));
        let idx2 = chunk.push_const(Value::Integer(24));

        assert_ne!(idx1, idx2);
    }
}
