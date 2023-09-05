// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use std::{collections::HashMap, rc::Rc};

use eyre::{eyre, Result};
use nil_syntax::{
    ast::{
        Apply, AttrSet, BinaryOp, BinaryOpKind, Expr, HasBindings, HasStringParts, IndentString,
        Lambda, LetIn, List, Literal, LiteralKind, Ref, Select, String as StringAst, StringPart,
        UnaryOp, UnaryOpKind,
    },
    parser,
};
use tracing::{field, Span};

mod context;
mod value;

use crate::eval::context::Context;
use crate::eval::value::Value;

pub fn code(code: &str) -> Result<Value> {
    let code = code.trim();

    let code_span = tracing::debug_span!("processing code", code);

    let expr = {
        tracing::debug_span!(parent: &code_span, "parsing code").in_scope(|| {
            parser::parse_file(code)
                .root()
                .expr()
                .ok_or_else(|| eyre!("No root-expression"))
        })?
    };

    tracing::debug_span!(parent: &code_span, "evaluating code")
        .in_scope(|| eval_expr(&expr, &Default::default()))
}

fn eval_expr(expr: &Expr, ctx: &Context) -> Result<Value> {
    match expr {
        Expr::Apply(f) => eval_apply(f, ctx),
        Expr::AttrSet(set) => eval_attr_set(set, ctx),
        Expr::BinaryOp(bo) => eval_binop(bo, ctx),
        Expr::IndentString(is) => eval_indent_str(is, ctx),
        Expr::LetIn(li) => eval_let_in(li, ctx),
        Expr::Literal(lit) => eval_literal(lit, ctx),
        Expr::Ref(rf) => eval_ref(rf, ctx),
        Expr::Select(s) => eval_select(s, ctx),
        Expr::String(s) => eval_str(s, ctx),
        Expr::UnaryOp(uo) => eval_unary_op(uo, ctx),
        Expr::Lambda(l) => eval_lambda(l, ctx),
        Expr::List(l) => eval_list(l, ctx),
        expr => Err(eyre!("expr: {:?}", expr)),
    }
}

fn eval_list(l: &List, ctx: &Context) -> Result<Value> {
    let list_code = l
        .l_brack_token()
        .and_then(|t| t.parent())
        .map(|p| p.text())
        .ok_or(eyre!("No list"))?
        .to_string();

    tracing::debug_span!("eval_list", code = list_code, capacity = field::Empty,).in_scope(|| {
        let capacity = l.elements().count();

        Span::current().record("capacity", capacity);

        let mut result = Vec::with_capacity(capacity);

        for expr in l.elements() {
            result.push(eval_expr(&expr, ctx)?);
        }

        result.shrink_to_fit();

        Ok(Value::List(result))
    })
}

fn eval_lambda(l: &Lambda, ctx: &Context) -> Result<Value> {
    let lambda_code = l
        .colon_token()
        .and_then(|t| t.parent())
        .map(|p| p.text())
        .ok_or(eyre!("No funciotn"))?
        .to_string();

    tracing::debug_span!("eval_lambda", code = lambda_code).in_scope(|| {
        let l2 = l.clone();
        let ctx = ctx.clone();

        let arg = l
            .clone()
            .param()
            .ok_or_else(|| eyre!("Missing param"))?
            .name()
            .ok_or_else(|| eyre!("Missing name"))?
            .token()
            .ok_or_else(|| eyre!("Missing token"))?;

        Ok(Value::Lambda(
            Rc::new(move |val| {
                let own_ctx = Context::new_with_parent(&ctx);

                let mut scope: HashMap<String, Value> = HashMap::new();

                scope.insert(arg.text().to_owned(), val);

                {
                    let mut ctx_builder = own_ctx.0.lock();
                    ctx_builder.values = scope;
                }

                eval_expr(&l2.body().ok_or_else(|| eyre!("Missing body"))?, &own_ctx)
            }),
            l.clone(),
        ))
    })
}

fn eval_apply(f: &Apply, ctx: &Context) -> Result<Value> {
    let apply_code = "<->";

    tracing::debug_span!("eval_apply", code = apply_code).in_scope(|| {
        let fun = f.function().ok_or_else(|| eyre!("Missing function"))?;

        let callable = match fun {
            Expr::Ref(ref rf) => {
                let name = rf.token().ok_or_else(|| eyre!("Missing token"))?;
                let func = ctx.resolve(name.text())?;
                match func {
                    Value::Thunk(expr, ctx) => eval_expr(&expr, &ctx),
                    lambda @ Value::Lambda(_, _) => Ok(lambda),
                    _ => Err(eyre!("Function must be a thunk or attrset")),
                }
            }
            expr => todo!("eval fun: expr {expr:?}"),
        }?;

        match dbg!(&callable) {
            Value::Lambda(l, _) => {
                let arg = eval_expr(&f.argument().ok_or_else(|| eyre!("Missing argument"))?, ctx)?;
                l(arg)
            }
            _ => todo!("eval apply: callable {callable:?}"),
        }
    })

    // Err(eyre!("apply not implemented yet"))
}

fn eval_select(s: &Select, ctx: &Context) -> Result<Value> {
    let select_code = s
        .dot_token()
        .and_then(|t| t.parent())
        .map(|p| p.text())
        .ok_or(eyre!("No funciotn"))?
        .to_string();

    tracing::debug_span!("eval_apply", code = select_code).in_scope(|| {
        let attrpath = s
            .attrpath()
            .ok_or_else(|| eyre!("Missing attrpath"))?
            .attrs()
            .map(|attr| match attr {
                nil_syntax::ast::Attr::Dynamic(_) => todo!(),
                nil_syntax::ast::Attr::Name(name) => name.token().unwrap().text().into(),
                nil_syntax::ast::Attr::String(_) => todo!(),
            })
            .collect::<Vec<String>>();

        if attrpath.len() > 1 {
            Err(eyre!("Nested-attrpath is not yet implemented"))?;
        }

        let set = s.set().ok_or_else(|| eyre!("Missing set"))?;

        match eval_expr(&set, ctx)? {
            Value::AttrSet(set) => match set.get(&attrpath[0]) {
                Some(Value::Thunk(_expr, ctx)) => ctx.resolve(attrpath[0].clone()),
                Some(value) => Ok(value.clone()),
                None => match s.default_expr() {
                    Some(default) => eval_expr(&default, ctx),
                    None => Err(eyre!("Missing attribute: {}", attrpath[0])),
                },
            },
            value => Err(eyre!("Selecting from {:?}", value)),
        }
    })
}

fn eval_let_in(li: &LetIn, ctx: &Context) -> Result<Value> {
    let let_in_code = li
        .let_token()
        .and_then(|t| t.parent())
        .map(|t| t.text().to_string())
        .unwrap();

    tracing::debug_span!("eval_let_in", code = let_in_code).in_scope(|| {
        let own_ctx = Context::new_with_parent(ctx);

        let mut scope: HashMap<String, Value> = HashMap::new();

        for binding in li.bindings() {
            use nil_syntax::ast::Binding;
            let (name, value_expr) = match binding {
                Binding::Inherit(_) => Err(eyre!("Inherit is not implemented"))?,
                Binding::AttrpathValue(path) => (
                    path.attrpath()
                        .ok_or_else(|| eyre!("Missing attrpath for binding"))?
                        .attrs()
                        .map(|attr| match attr {
                            nil_syntax::ast::Attr::Dynamic(_) => todo!(),
                            nil_syntax::ast::Attr::Name(name) => {
                                name.token().unwrap().text().into()
                            }
                            nil_syntax::ast::Attr::String(_) => todo!(),
                        })
                        .collect::<Vec<String>>(),
                    path.value()
                        .ok_or_else(|| eyre!("Missing value for binding"))?,
                ),
            };

            if name.len() > 1 {
                Err(eyre!("Nested-attrpath is not yet implemented"))?;
            }

            // scope[&name[0]] = Value::Thunk(value_expr.clone());
            scope.insert(
                name[0].clone(),
                Value::Thunk(value_expr.clone(), own_ctx.clone()),
            );
        }

        {
            let mut ctx_builder = own_ctx.0.lock();
            ctx_builder.values = scope;
        }

        li.body()
            .map(|body| eval_expr(&body, &own_ctx))
            .unwrap_or_else(|| Ok(Value::Bool(false)))
    })
}

fn eval_attr_set(set: &AttrSet, ctx: &Context) -> Result<Value> {
    let attr_set_code = set
        .l_curly_token()
        .and_then(|t| t.parent())
        .map(|t| t.text())
        .unwrap()
        .to_string();

    tracing::debug_span!("eval_attr_set", code = attr_set_code).in_scope(|| {
        let own_ctx = Context::new_with_parent(ctx);

        let mut scope: HashMap<String, Value> = HashMap::new();

        for binding in set.bindings() {
            use nil_syntax::ast::Binding;
            let (name, value_expr) = match binding {
                Binding::Inherit(_) => Err(eyre!("Inherit is not implemented"))?,
                Binding::AttrpathValue(path) => (
                    path.attrpath()
                        .ok_or_else(|| eyre!("Missing attrpath for binding"))?
                        .attrs()
                        .map(|attr| match attr {
                            nil_syntax::ast::Attr::Dynamic(_) => todo!(),
                            nil_syntax::ast::Attr::Name(name) => {
                                name.token().unwrap().text().into()
                            }
                            nil_syntax::ast::Attr::String(_) => todo!(),
                        })
                        .collect::<Vec<String>>(),
                    path.value()
                        .ok_or_else(|| eyre!("Missing value for binding"))?,
                ),
            };

            if name.len() > 1 {
                Err(eyre!("Nested-attrpath is not yet implemented"))?;
            }

            // scope[&name[0]] = Value::Thunk(value_expr.clone());
            scope.insert(
                name[0].clone(),
                Value::Thunk(value_expr.clone(), own_ctx.clone()),
            );
        }

        {
            let mut ctx_builder = own_ctx.0.lock();
            ctx_builder.values = scope.clone();
        }

        Ok(Value::AttrSet(scope))
    })
}

fn eval_str(s: &StringAst, _ctx: &Context) -> Result<Value> {
    let string_code = s
        .start_dquote_token()
        .and_then(|t| t.parent())
        .map(|t| t.text())
        .unwrap()
        .to_string();

    tracing::debug_span!("eval_str", code = string_code, string = field::Empty).in_scope(|| {
        let mut result = String::new();

        for part in s.string_parts() {
            match part {
                StringPart::Fragment(f) => result.push_str(f.text()),
                StringPart::Escape(e) => todo!("Escape {:?}", e),
                StringPart::Dynamic(d) => todo!("Dynamic {:?}", d),
            }

            Span::current().record("string", &result);
        }

        result.shrink_to_fit();

        Ok(Value::String(result))
    })
}

fn eval_indent_str(is: &IndentString, _ctx: &Context) -> Result<Value> {
    let string_code = is
        .start_quote2_token()
        .and_then(|t| t.parent())
        .map(|t| t.text())
        .unwrap()
        .to_string();

    let start = is.start_quote2_token().unwrap().text_range().start();
    let stop = is.end_quote2_token().unwrap().text_range().end();
    let capacity_guess = stop - start;

    tracing::debug_span!(
        "eval_str",
        code = string_code,
        string = field::Empty,
        start = usize::from(start),
        stop = usize::from(stop),
        capacity_guess = usize::from(capacity_guess)
    )
    .in_scope(|| {
        let mut result = String::with_capacity(capacity_guess.into());

        for part in is.string_parts() {
            match part {
                StringPart::Fragment(f) => result.push_str(f.text()),
                StringPart::Escape(e) => todo!("Escape {:?}", e),
                StringPart::Dynamic(d) => todo!("Dynamic {:?}", d),
            }

            Span::current().record("string", &result);
        }

        result = textwrap::dedent(&result);

        Ok(Value::String(result))
    })
}

fn eval_ref(rf: &Ref, ctx: &Context) -> Result<Value> {
    let ref_code = rf.token().map(|t| t.text().to_string()).unwrap();

    tracing::debug_span!("eval_ref", code = ref_code).in_scope(|| match ref_code.as_str() {
        "true" => Ok(Value::Bool(true)),
        "false" => Ok(Value::Bool(false)),
        name => Ok(ctx.resolve(name)?),
    })
}

fn eval_literal(lit: &Literal, _ctx: &Context) -> Result<Value> {
    let lit_code = lit.token().map(|t| t.text().to_string()).unwrap();

    tracing::debug_span!("eval_literal", code = lit_code).in_scope(|| match lit.kind().unwrap() {
        LiteralKind::Int => Ok(Value::Integer(lit.token().unwrap().text().parse::<i64>()?)),
        LiteralKind::Float => Ok(Value::Float(lit.token().unwrap().text().parse::<f64>()?)),
        LiteralKind::Path => Ok(Value::Path(lit.token().unwrap().text().into())),
        token => Err(eyre!("eval literal: {:?}", token)),
    })
}

fn eval_binop(bin_op: &BinaryOp, ctx: &Context) -> Result<Value> {
    let code = bin_op
        .op_token()
        .and_then(|t| t.parent())
        .map(|p| p.text())
        .ok_or(eyre!("No binop"))?
        .to_string();

    tracing::debug_span!("eval_binop", code).in_scope(|| {
        let lhs = eval_expr(&bin_op.lhs().ok_or_else(|| eyre!("Missing LHS"))?, ctx)?;
        let rhs = eval_expr(&bin_op.rhs().ok_or_else(|| eyre!("Missing RHS"))?, ctx)?;
        match (lhs, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => match bin_op.op_kind().unwrap() {
                BinaryOpKind::Add => Ok(Value::Integer(lhs + rhs)),
                BinaryOpKind::Sub => Ok(Value::Integer(lhs - rhs)),
                BinaryOpKind::Mul => Ok(Value::Integer(lhs * rhs)),
                BinaryOpKind::Div => Ok(Value::Integer(lhs / rhs)),
                op_token => todo!("op_token: {:?}", op_token),
            },
            (Value::Integer(lhs), Value::Float(rhs)) => match bin_op.op_kind().unwrap() {
                BinaryOpKind::Add => Ok(Value::Float(lhs as f64 + rhs)),
                BinaryOpKind::Sub => Ok(Value::Float(lhs as f64 - rhs)),
                BinaryOpKind::Mul => Ok(Value::Float(lhs as f64 * rhs)),
                BinaryOpKind::Div => Ok(Value::Float(lhs as f64 / rhs)),
                op_token => todo!("op_token: {:?}", op_token),
            },
            (Value::Float(lhs), Value::Integer(rhs)) => match bin_op.op_kind().unwrap() {
                BinaryOpKind::Add => Ok(Value::Float(lhs + rhs as f64)),
                BinaryOpKind::Sub => Ok(Value::Float(lhs - rhs as f64)),
                BinaryOpKind::Mul => Ok(Value::Float(lhs * rhs as f64)),
                BinaryOpKind::Div => Ok(Value::Float(lhs / rhs as f64)),
                op_token => todo!("op_token: {:?}", op_token),
            },
            (Value::Float(lhs), Value::Float(rhs)) => match bin_op.op_kind().unwrap() {
                BinaryOpKind::Add => Ok(Value::Float(lhs + rhs)),
                BinaryOpKind::Sub => Ok(Value::Float(lhs - rhs)),
                BinaryOpKind::Mul => Ok(Value::Float(lhs * rhs)),
                BinaryOpKind::Div => Ok(Value::Float(lhs / rhs)),
                op_token => todo!("op_token: {:?}", op_token),
            },
            (lhs, rhs) => todo!("lhs: {:?}, rhs: {:?}", lhs, rhs),
        }
    })
}

fn eval_unary_op(unary_op: &UnaryOp, ctx: &Context) -> Result<Value> {
    let unary_code = unary_op
        .op_token()
        .and_then(|t| t.parent())
        .map(|p| p.text())
        .ok_or(eyre!("No unary op"))?
        .to_string();

    tracing::debug_span!("eval_unary_op", code = unary_code).in_scope(|| {
        let rhs = eval_expr(
            &unary_op
                .arg()
                .ok_or_else(|| eyre!("Missing argument for unary operator"))?,
            ctx,
        )?;
        match rhs {
            Value::Integer(val) => match unary_op.op_kind().unwrap() {
                UnaryOpKind::Negate => Ok(Value::Integer(-val)),
                UnaryOpKind::Not => Err(eyre!("Not is not implemented")),
            },
            Value::Float(val) => match unary_op.op_kind().unwrap() {
                UnaryOpKind::Negate => Ok(Value::Float(-val)),
                UnaryOpKind::Not => Err(eyre!("Not is not implemented")),
            },
            Value::Bool(val) => match unary_op.op_kind().unwrap() {
                UnaryOpKind::Not => Ok(Value::Bool(!val)),
                UnaryOpKind::Negate => Err(eyre!("Negate is not implemented")),
            },
            Value::Path(_) => Err(eyre!(
                "Operation {:?} is not implemented for paths",
                unary_op
            )),
            Value::String(_) => Err(eyre!(
                "Operation {:?} is not implemented for strings",
                unary_op
            )),
            Value::AttrSet(_) => Err(eyre!(
                "Operation {:?} is not implemented for attrsets",
                unary_op
            )),
            Value::List(_) => Err(eyre!(
                "Operation {:?} is not implemented for lists",
                unary_op
            )),
            Value::Thunk(_, _) => Err(eyre!(
                "Operation {:?} is not implemented for thunks",
                unary_op
            )),
            Value::Lambda(_, _) => Err(eyre!(
                "Operation {:?} is not implemented for lambdas",
                unary_op
            )),
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    const INDENT_STRING: &str = r#"''
    foo
    bar
    ''"#;

    #[rstest]
    #[case::two_summands("1 + 2", Value::Integer(3))]
    #[case::three_summands("1 + 2 + 3", Value::Integer(6))]
    #[case::two_no_space("1+2", Value::Integer(3))]
    #[case::three_no_space("1+2+3", Value::Integer(6))]
    #[case::lhs_neg("-1 + 2", Value::Integer(1))]
    #[case::rhs_neg("1 + -2", Value::Integer(-1))]
    #[case::lhs_float("1.0 + 2", Value::Float(3.0))]
    #[case::rhs_float("1 + 2.0", Value::Float(3.0))]
    #[case::both_float("1.0 + 2.0", Value::Float(3.0))]
    fn simple_additions(#[case] code: &str, #[case] expected: Value) {
        assert_eq!(super::code(code).unwrap(), expected);
    }

    #[rstest]
    #[case::two_summands("1 - 2", Value::Integer(-1))]
    #[case::three_summands("1 - 2 - 3", Value::Integer(-4))]
    #[case::two_no_space("1-2", Value::Integer(-1))]
    #[case::three_no_space("1-2-3", Value::Integer(-4))]
    #[case::lhs_neg("-1 - 2", Value::Integer(-3))]
    #[case::rhs_neg("1 - -2", Value::Integer(3))]
    #[case::lhs_float("1.0 - 2", Value::Float(-1.0))]
    #[case::rhs_float("1 - 2.0", Value::Float(-1.0))]
    #[case::both_float("1.0 - 2.0", Value::Float(-1.0))]
    fn simple_substractions(#[case] code: &str, #[case] expected: Value) {
        assert_eq!(super::code(code).unwrap(), expected);
    }

    #[rstest]
    #[case::two_summands("1 * 2", Value::Integer(2))]
    #[case::three_summands("1 * 2 * 3", Value::Integer(6))]
    #[case::two_no_space("1*2", Value::Integer(2))]
    #[case::three_no_space("1*2*3", Value::Integer(6))]
    #[case::lhs_neg("-1 * 2", Value::Integer(-2))]
    #[case::rhs_neg("1 * -2", Value::Integer(-2))]
    #[case::lhs_float("1.0 * 2", Value::Float(2.0))]
    #[case::rhs_float("1 * 2.0", Value::Float(2.0))]
    #[case::both_float("1.0 * 2.0", Value::Float(2.0))]
    fn simple_multiplication(#[case] code: &str, #[case] expected: Value) {
        assert_eq!(super::code(code).unwrap(), expected);
    }

    #[rstest]
    #[case::two_summands("4 / 2", Value::Integer(2))]
    #[case::three_summands("12 / 2 / 3", Value::Integer(2))]
    #[case::lhs_neg("-4 / 2", Value::Integer(-2))]
    #[case::rhs_neg("4 / -2", Value::Integer(-2))]
    #[case::lhs_float("1.0 / 2", Value::Float(0.5))]
    #[case::rhs_float("1 / 2.0", Value::Float(0.5))]
    #[case::both_float("1.0 / 2.0", Value::Float(0.5))]
    // Regular nix would parse this as a path, not a division.
    // #[case::two_no_space("4/2", Value::Integer(2))]
    // #[case::three_no_space("12/2/3", Value::Integer(2))]
    fn simple_division(#[case] code: &str, #[case] expected: Value) {
        assert_eq!(super::code(code).unwrap(), expected);
    }

    #[rstest]
    #[case("true", Value::Bool(true))]
    #[case("false", Value::Bool(false))]
    #[case::negated("!true", Value::Bool(false))]
    fn simple_booleans(#[case] code: &str, #[case] expected: Value) {
        assert_eq!(super::code(code).unwrap(), expected);
    }

    #[rstest]
    #[case("1", Value::Integer(1))]
    #[case("123341231312", Value::Integer(123341231312))]
    #[case("-3489237489", Value::Integer(-3489237489))]
    #[case("0", Value::Integer(0))]
    #[case("1.0", Value::Float(1.0))]
    #[case("123341231312.234", Value::Float(123341231312.234))]
    #[case("1.0e21", Value::Float(1e21))]
    #[case("1.0e-21", Value::Float(1e-21))]
    #[case("-1.0e21", Value::Float(-1e21))]
    #[case("-1.0e-21", Value::Float(-1e-21))]
    #[case("a/b", Value::Path("a/b".into()))]
    #[case("./a/b", Value::Path("./a/b".into()))]
    #[case("/a/b", Value::Path("/a/b".into()))]
    #[case("~/a/b", Value::Path("~/a/b".into()))]
    #[case("\"~/a/b\"", Value::String("~/a/b".into()))]
    #[case(INDENT_STRING, Value::String("\nfoo\nbar\n".into()))]
    #[case("{}", Value::AttrSet(HashMap::new()))]
    fn simple_literals(#[case] code: &str, #[case] expected: Value) {
        assert_eq!(super::code(code).unwrap(), expected);
    }

    #[rstest]
    #[case::let_in("let a = 1; in a", Value::Integer(1))]
    #[case::attr_set("{a = 1;}.a", Value::Integer(1))]
    #[case::combine("let s = {a = 1;}; in s.a", Value::Integer(1))]
    #[case::or("let s = {a = 1;}; in s.b or false", Value::Bool(false))]
    #[case::nested_let("let a = let b = 1; in b; in a", Value::Integer(1))]
    fn variables(#[case] code: &str, #[case] expected: Value) {
        assert_eq!(super::code(code).unwrap(), expected);
    }

    #[rstest]
    #[case::fun_def_id("let id = a: a; in id 1", Value::Integer(1))]
    #[case::fun_def_one_arg("let f = a: 1 + a; in f 1", Value::Integer(2))]
    // #[case::fun_def_two_arg("let f = a: b: b + a; in f 1 1", Value::Integer(1))]
    fn functions(#[case] code: &str, #[case] expected: Value) {
        assert_eq!(super::code(code).unwrap(), expected);
    }

    #[rstest]
    #[case::empty("[]", Value::List(Vec::new()))]
    #[case::one_element("[1]", Value::List(vec![Value::Integer(1)]))]
    #[case::two_elements(r#"[1 "2"]"#, Value::List(vec![Value::Integer(1), Value::String("2".into())]))]
    fn lists(#[case] code: &str, #[case] expected: Value) {
        assert_eq!(super::code(code).unwrap(), expected);
    }
}
