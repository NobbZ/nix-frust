// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

mod chunk;
mod opcode;
mod scope;
mod span;

use std::path::PathBuf;

use rnix::{self, ast::Expr};

use crate::compiler::chunk::Chunk;
use crate::compiler::opcode::OpCode;
use crate::compiler::scope::{LocalIdx, Scope};
use crate::compiler::span::ToSpan;
use crate::eval::value::Value;

#[derive(Debug)]
pub(crate) struct Context {
    scope: Scope,
    chunk: Chunk,
}

impl Context {
    fn new() -> Self {
        Self {
            chunk: Chunk::new(),
            scope: Default::default(),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Compiler {
    pub(crate) contexts: Vec<Context>,
    pub(crate) warnings: Vec<()>,
    pub(crate) errors: Vec<()>,
    pub(crate) expr: Expr,
}

impl Compiler {
    pub(crate) fn new(code: &str, filename: Option<&str>) -> Self {
        let parsed = rnix::ast::Root::parse(code);

        if !parsed.errors().is_empty() {
            todo!()
        }

        let expr = parsed.tree().expr().unwrap();

        Self::from_expr(expr, filename)
    }

    fn from_expr(expr: Expr, _filename: Option<&str>) -> Self {
        Self {
            contexts: vec![Context::new()],
            warnings: vec![],
            errors: vec![],

            expr,
        }
    }
}

impl Compiler {
    pub(crate) fn scope_mut(&mut self) -> &mut Scope {
        &mut self.context_mut().scope
    }

    pub(crate) fn context_mut(&mut self) -> &mut Context {
        debug_assert!(!self.contexts.is_empty());
        let idx = self.contexts.len() - 1;
        &mut self.contexts[idx]
    }
}

impl Compiler {
    pub(crate) fn compile(&mut self, expr: Expr, _slot: LocalIdx) {
        match expr {
            Expr::Apply(_) => todo!(),
            Expr::Assert(_) => todo!(),
            Expr::AttrSet(_) => todo!(),
            Expr::BinOp(_) => todo!(),
            Expr::Error(_) => todo!(),
            Expr::HasAttr(_) => todo!(),
            Expr::Ident(_) => todo!(),
            Expr::IfElse(_) => todo!(),
            Expr::Lambda(_) => todo!(),
            Expr::LegacyLet(_) => todo!(),
            Expr::LetIn(_) => todo!(),
            Expr::List(_) => todo!(),
            Expr::Literal(literal) => self.compile_literal(literal),
            Expr::Paren(_) => todo!(),
            Expr::Path(_) => todo!(),
            Expr::Root(_) => todo!(),
            Expr::Select(_) => todo!(),
            Expr::Str(_) => todo!(),
            Expr::UnaryOp(_) => todo!(),
            Expr::With(_) => todo!(),
        }
    }

    fn compile_literal(&mut self, node: rnix::ast::Literal) {
        let value = match node.kind() {
            rnix::ast::LiteralKind::Float(f) => Value::Float(f.value().unwrap()),
            rnix::ast::LiteralKind::Integer(i) => Value::Integer(i.value().unwrap()),
            rnix::ast::LiteralKind::Uri(_) => todo!(),
        };

        self.emit_const(value, node);
    }
}

impl Compiler {
    fn emit_const<N>(&mut self, value: Value, node: N)
    where
        N: ToSpan,
    {
        let idx = self.contexts.last_mut().unwrap().chunk.push_const(value);
        self.emit_op(OpCode::Const(idx), node);
    }

    fn emit_op<N>(&mut self, opcode: OpCode, _node: N)
    where
        N: ToSpan,
    {
        self.contexts.last_mut().unwrap().chunk.push_op(opcode);
    }

    #[allow(dead_code)]
    fn emit_force<N>(&mut self, node: N)
    where
        N: ToSpan,
    {
        self.emit_op(OpCode::Force, node);
    }
}

#[allow(dead_code)]
pub(crate) fn compile<P>(expr: Expr, _location: Option<P>) -> Compiler
where
    P: Into<PathBuf>,
{
    let mut c = Compiler::from_expr(expr.clone(), None);

    let root_slot = c.scope_mut().declare_phantom(false);
    c.compile(expr.clone(), root_slot);

    c.emit_force(expr.clone());
    c.emit_op(OpCode::Return, expr);

    c
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    // use test_generator::test_resources;

    #[rstest]
    #[case::integer("1")]
    #[case::float("1.0")]
    // #[case::string(r#""foo""#)]
    // #[case::path(r#"/foo/bar"#)]
    // #[case::indent_string("''foo''")]
    // #[case::list("[1 2 3]")]
    // #[case::attr_set("{ foo = 1; }")]
    fn simple_literals(#[case] code: &str) {
        let mut compiler = Compiler::new(code, None);
        let root_slot = compiler.scope_mut().declare_phantom(false);
        compiler.compile(compiler.expr.clone(), root_slot);

        assert_eq!(compiler.warnings.len(), 0);
        assert_eq!(compiler.errors.len(), 0);
    }

    // #[test_resources("tests/nix-tests/parse-okay-*.nix")]
    // fn parse_okay(filename: &str) {
    //     let code = std::fs::read_to_string(filename).expect("failed to read test file");

    //     let mut compiler = Compiler::new(&code, Some(filename));
    //     let expr = compiler.expr.clone();
    //     compiler.compile(expr);

    //     assert_eq!(compiler.warnings.len(), 0);
    //     assert_eq!(compiler.errors.len(), 0);
    // }

    // #[test_reources("tests/nix-tests/parse-fail-*.nix")]
}
