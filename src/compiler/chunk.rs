// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use crate::compiler::opcode::OpCode;
use crate::eval::value::Value;

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct ConstIdx(usize);

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct CodeIdx(usize);

#[derive(Debug)]
pub(crate) struct Chunk {
    pub opcodes: Vec<OpCode>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub(crate) fn new() -> Self {
        Self {
            opcodes: vec![],
            constants: vec![],
        }
    }

    pub(crate) fn push_op(&mut self, op: OpCode) -> CodeIdx {
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
