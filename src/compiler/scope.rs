// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use super::opcode::UpvalueIdx;

#[derive(Debug)]
#[allow(dead_code)]
enum LocalName {
    /// Normally declared local with a statically known name.
    Ident(String),

    /// Phantom stack value (e.g. attribute set used for `with`) that
    /// must be accounted for to calculate correct stack offsets.
    Phantom,
}

/// Represents a single local already known to the compiler.
#[derive(Debug)]
#[allow(dead_code)]
pub struct Local {
    /// Identifier of this local. This is always a statically known
    /// value (Nix does not allow dynamic identifier names in locals),
    /// or a "phantom" value not accessible by users.
    name: LocalName,

    /// Source span at which this local was declared.
    // pub span: codemap::Span,

    /// Scope depth of this local.
    pub depth: usize,

    /// Is this local initialised?
    pub initialised: bool,

    /// Is this local known to have been used at all?
    pub used: bool,

    /// Does this local need to be finalised after the enclosing scope
    /// is completely constructed?
    pub needs_finaliser: bool,

    /// Does this local's upvalues contain a reference to itself?
    pub must_thunk: bool,
}

/// Represents the different ways in which upvalues can be captured in
/// closures or thunks.
#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(dead_code)]
pub enum UpvalueKind {
    /// This upvalue captures a local from the stack.
    Local(LocalIdx),

    /// This upvalue captures an enclosing upvalue.
    Upvalue(UpvalueIdx),
}

/// The index of a local in the scope's local array at compile time.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd)]
pub struct LocalIdx(usize);

#[derive(Clone, Debug)]
pub struct Upvalue {
    pub kind: UpvalueKind,
    pub span: codemap::Span,
}

/// Helper struct for indexing over `Scope::locals` by name.
#[derive(Debug)]
#[allow(dead_code)]
enum ByName {
    Single(LocalIdx),
    Shadowed(Vec<LocalIdx>),
}

#[derive(Default, Debug)]
#[allow(dead_code)]
pub(crate) struct Scope {
    locals: Vec<Local>,
    pub upvalues: Vec<Upvalue>,

    by_name: std::collections::HashMap<String, ByName>,

    scope_depth: usize,
}

impl Scope {
    /// Declare a local variable that occupies a stack slot and should
    /// be accounted for, but is not directly accessible by users
    /// (e.g. attribute sets used for `with`).
    pub fn declare_phantom(&mut self, initialised: bool) -> LocalIdx {
        let idx = self.locals.len();
        self.locals.push(Local {
            initialised,
            name: LocalName::Phantom,
            depth: self.scope_depth,
            needs_finaliser: false,
            must_thunk: false,
            used: true,
        });

        LocalIdx(idx)
    }
}
