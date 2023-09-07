// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use crate::compiler::chunk::ConstIdx;

/// Index of an upvalue within a closure's bound-variable upvalue
/// list.  This is an absolute index into the Upvalues of the
/// CallFrame containing the opcode which contains this UpvalueIdx.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UpvalueIdx(pub usize);

#[derive(Debug)]
pub(crate) enum OpCode {
    Const(ConstIdx),
    Force,
    Return,
}
