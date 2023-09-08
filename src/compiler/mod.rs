// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

pub(crate) mod chunk;
pub(crate) mod opcode;
pub(crate) mod scope;
pub(crate) mod span;

use std::path::PathBuf;
use std::rc::Rc;

use codemap::Span;
use rnix::ast::{self, Expr};

use crate::compiler::chunk::Chunk;
use crate::compiler::opcode::{Count, OpCode};
use crate::compiler::scope::{LocalIdx, Scope};
use crate::compiler::span::{LightSpan, ToSpan};
use crate::eval::value::Value;
use crate::values::{Closure, Lambda, Thunk};

#[derive(Debug)]
/// Represents the lambda currently being compiled.
struct Context {
    lambda: Lambda,
    scope: Scope,
    captures_with_stack: bool,
    unthunk: bool,
}

impl Context {
    fn new() -> Self {
        Context {
            lambda: Lambda::default(),
            scope: Default::default(),
            captures_with_stack: false,
            unthunk: false,
        }
    }

    fn inherit(&self) -> Self {
        Context {
            lambda: Lambda::default(),
            scope: self.scope.inherit(),
            captures_with_stack: false,
            unthunk: false,
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
        let parsed = ast::Root::parse(code);

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
    fn scope(&self) -> &Scope {
        &self.context().scope
    }

    fn context(&self) -> &Context {
        &self.contexts[self.contexts.len() - 1]
    }

    fn chunk(&mut self) -> &mut Chunk {
        &mut self.context_mut().lambda.chunk
    }

    pub(crate) fn scope_mut(&mut self) -> &mut Scope {
        &mut self.context_mut().scope
    }

    pub(crate) fn context_mut(&mut self) -> &mut Context {
        debug_assert!(!self.contexts.is_empty());
        let idx = self.contexts.len() - 1;
        &mut self.contexts[idx]
    }

    /// Open a new lambda context within which to compile a function,
    /// closure or thunk.
    fn new_context(&mut self) {
        self.contexts.push(self.context().inherit());
    }
}

impl Compiler {
    pub(crate) fn compile(&mut self, slot: LocalIdx, expr: Expr) {
        match &expr {
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
            Expr::Str(string) => self.compile_string(slot, string),
            Expr::UnaryOp(_) => todo!(),
            Expr::With(_) => todo!(),
        }
    }

    fn compile_literal(&mut self, node: &ast::Literal) {
        let value = match node.kind() {
            ast::LiteralKind::Float(f) => Value::Float(f.value().unwrap()),
            ast::LiteralKind::Integer(i) => Value::Integer(i.value().unwrap()),
            ast::LiteralKind::Uri(_) => todo!(),
        };

        self.emit_const(value, node);
    }

    /// Helper that compiles the given string parts strictly. The caller
    /// (`compile_str`) needs to figure out if the result of compiling this
    /// needs to be thunked or not.
    fn compile_str_parts(&mut self, slot: LocalIdx, parent_node: &ast::Str, parts: Vec<ast::InterpolPart<String>>) {
        // The string parts are produced in literal order, however
        // they need to be reversed on the stack in order to
        // efficiently create the real string in case of
        // interpolation.
        for part in parts.iter().rev() {
            match part {
                // Interpolated expressions are compiled as normal and
                // dealt with by the VM before being assembled into
                // the final string. We need to coerce them here,
                // so OpInterpolate definitely has a string to consume.
                ast::InterpolPart::Interpolation(ipol) => {
                    self.compile(slot, ipol.expr().unwrap());
                    // implicitly forces as well
                    self.emit_op(OpCode::CoerceToString, ipol);
                }

                ast::InterpolPart::Literal(lit) => {
                    self.emit_const(Value::String(lit.as_str().into()), parent_node);
                }
            }
        }

        if parts.len() != 1 {
            self.emit_op(OpCode::Interpolate(Count(parts.len())), parent_node);
        }
    }

    fn compile_string(&mut self, slot: LocalIdx, node: &ast::Str) {
        let parts = node.normalized_parts();

        // We need to thunk string expressions if they are the result of
        // interpolation. A string that only consists of a single part (`"${foo}"`)
        // can't desugar to the enclosed expression (`foo`) because we need to
        // coerce the result to a string value. This would require forcing the
        // value of the inner expression, so we need to wrap it in another thunk.
        if parts.len() != 1 || matches!(&parts[0], ast::InterpolPart::Interpolation(_)) {
            self.thunk(slot, node, move |c, s| {
                c.compile_str_parts(s, node, parts);
            });
        } else {
            self.compile_str_parts(slot, node, parts);
        }
    }

    fn thunk<N, F>(&mut self, outer_slot: LocalIdx, node: &N, content: F)
    where
        N: ToSpan,
        F: FnOnce(&mut Compiler, LocalIdx),
    {
        self.compile_lambda_or_thunk(true, outer_slot, node, content)
    }

    /// Compile an expression into a runtime closure or thunk
    fn compile_lambda_or_thunk<N, F>(&mut self, is_suspended_thunk: bool, outer_slot: LocalIdx, node: &N, content: F)
    where
        N: ToSpan,
        F: FnOnce(&mut Compiler, LocalIdx),
    {
        let name = self.scope()[outer_slot].name();
        self.new_context();

        // Set the (optional) name of the current slot on the lambda that is
        // being compiled.
        self.context_mut().lambda.name = name;

        let span = self.to_span(node);
        let slot = self.scope_mut().declare_phantom(span, false);
        self.scope_mut().begin_scope();

        content(self, slot);
        self.cleanup_scope(node);

        // TODO: determine and insert enclosing name, if available.

        // Pop the lambda context back off, and emit the finished
        // lambda as a constant.
        let mut compiled = self.contexts.pop().unwrap();

        // The compiler might have decided to unthunk, i.e. raise the compiled
        // code to the parent context. In that case we do so and return right
        // away.
        if compiled.unthunk && is_suspended_thunk {
            self.chunk().extend(compiled.lambda.chunk);
            return;
        }

        // Emit an instruction to inform the VM that the chunk has ended.
        compiled
            .lambda
            .chunk
            .push_op(OpCode::Return, self.to_span(node));

        // Capturing the with stack counts as an upvalue, as it is
        // emitted as an upvalue data instruction.
        if compiled.captures_with_stack {
            compiled.lambda.upvalue_count += 1;
        }

        let lambda = Rc::new(compiled.lambda);

        // If no upvalues are captured, emit directly and move on.
        if lambda.upvalue_count == 0 {
            self.emit_const(
                if is_suspended_thunk {
                    Value::Thunk(Thunk::new_suspended(lambda, LightSpan::new_actual(span)))
                } else {
                    Value::Closure(Rc::new(Closure::new(lambda)))
                },
                node,
            );
            return;
        }

        // Otherwise, we need to emit the variable number of
        // operands that allow the runtime to close over the
        // upvalues and leave a blueprint in the constant index from
        // which the result can be constructed.
        let blueprint_idx = self.chunk().push_constant(Value::Blueprint(lambda));

        let code_idx = self.push_op(
            if is_suspended_thunk {
                OpCode::OpThunkSuspended(blueprint_idx)
            } else {
                OpCode::OpThunkClosure(blueprint_idx)
            },
            node,
        );

        self.emit_upvalue_data(
            outer_slot,
            node,
            compiled.scope.upvalues,
            compiled.captures_with_stack,
        );

        if !is_suspended_thunk && !self.scope()[outer_slot].needs_finaliser {
            if !self.scope()[outer_slot].must_thunk {
                // The closure has upvalues, but is not recursive.  Therefore no thunk is required,
                // which saves us the overhead of Rc<RefCell<>>
                self.chunk()[code_idx] = OpCode::OpClosure(blueprint_idx);
            } else {
                // This case occurs when a closure has upvalue-references to itself but does not need a
                // finaliser.  Since no OpFinalise will be emitted later on we synthesize one here.
                // It is needed here only to set [`Closure::is_finalised`] which is used for sanity checks.
                #[cfg(debug_assertions)]
                self.push_op(
                    OpCode::OpFinalise(self.scope().stack_index(outer_slot)),
                    &self.span_for(node),
                );
            }
        }
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

    pub(crate) fn to_span<S: ToSpan>(&self, node: &S) -> Span {
        node.to_span(&self.file)
    }

    /// Decrease scope depth of the current function and emit
    /// instructions to clean up the stack at runtime.
    fn cleanup_scope<N: ToSpan>(&mut self, node: &N) {
        // When ending a scope, all corresponding locals need to be
        // removed, but the value of the body needs to remain on the
        // stack. This is implemented by a separate instruction.
        let (popcount, unused_spans) = self.scope_mut().end_scope();

        // TODO: emit warnings
        // for span in &unused_spans {
        //     self.emit_warning(span, WarningKind::UnusedBinding);
        // }

        if popcount > 0 {
            self.push_op(OpCode::OpCloseScope(Count(popcount)), node);
        }
    }
}

#[allow(dead_code)]
pub(crate) fn compile<P>(expr: &Expr, _location: Option<P>) -> Compiler
where
    P: Into<PathBuf>,
{
    let mut c = Compiler::from_expr(expr.clone(), None);

    let root_span = c.to_span(expr);
    let root_slot = c.scope_mut().declare_phantom(root_span, false);
    c.compile(expr, root_slot);

    c.emit_force(expr);
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
    #[case::string(r#""foo""#)]
    // #[case::path(r#"/foo/bar"#)]
    // #[case::indent_string("''foo''")]
    // #[case::list("[1 2 3]")]
    // #[case::attr_set("{ foo = 1; }")]
    fn simple_literals(#[case] code: &str) {
        let mut compiler = Compiler::new(code, None);
        let root_span = compiler.to_span(&compiler.expr);
        let root_slot = compiler.scope_mut().declare_phantom(root_span, false);
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
