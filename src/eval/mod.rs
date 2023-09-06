// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use std::{collections::HashMap, rc::Rc};

use eyre::{eyre, Result};
use rnix::{ast, parser};
use rowan::ast::AstNode;
use tracing::{field, Span};

mod context;
pub(crate) mod value;

use crate::eval::context::Context;
use crate::eval::value::Value;

pub fn code(code: &str) -> Result<Value> {
    let code = code.trim();

    let code_span = tracing::debug_span!("processing code", code);

    let expr = {
        tracing::debug_span!(parent: &code_span, "parsing code").in_scope(|| {
            let parsed = ast::Root::parse(code);

            if !parsed.errors().is_empty() {
                todo!()
            }

            parsed
                .tree()
                .expr()
                .ok_or_else(|| eyre!("No root-expression"))
        })?
    };

    tracing::debug_span!(parent: &code_span, "evaluating code")
        .in_scope(|| eval_expr(&expr, &Default::default()))
}

fn eval_expr(expr: &ast::Expr, ctx: &Context) -> Result<Value> {
    match expr {
        ast::Expr::Apply(f) => eval_apply(f, ctx),
        ast::Expr::AttrSet(set) => eval_attr_set(set, ctx),
        ast::Expr::BinOp(bo) => eval_bin_op(bo, ctx),
        ast::Expr::LetIn(li) => eval_let_in(li, ctx),
        ast::Expr::Literal(lit) => eval_literal(lit, ctx),
        ast::Expr::Select(s) => eval_select(s, ctx),
        ast::Expr::UnaryOp(uo) => eval_unary_op(uo, ctx),
        ast::Expr::Lambda(l) => eval_lambda(l, ctx),
        ast::Expr::List(l) => eval_list(l, ctx),
        expr => Err(eyre!("expr: {:?}", expr)),
    }
}

fn eval_list(l: &ast::List, ctx: &Context) -> Result<Value> {
    let list_code = l.syntax().text().to_string();

    tracing::debug_span!("eval_list", code = list_code, capacity = field::Empty,).in_scope(|| {
        let capacity = l.items().count();

        Span::current().record("capacity", capacity);

        let mut result = Vec::with_capacity(capacity);

        for expr in l.items() {
            result.push(eval_expr(&expr, ctx)?);
        }

        result.shrink_to_fit();

        Ok(Value::List(result))
    })
}

fn eval_lambda(l: &ast::Lambda, ctx: &Context) -> Result<Value> {
    let lambda_code = l.syntax().text().to_string();

    tracing::debug_span!("eval_lambda", code = lambda_code).in_scope(|| {
        let l2 = l.clone();
        let ctx = ctx.clone();

        let arg = match l.param().unwrap() {
            ast::Param::IdentParam(i) => {
                i.ident().unwrap().ident_token().unwrap().text().to_string()
            }
            ast::Param::Pattern(_) => todo!(),
        };

        Ok(Value::Lambda(
            Rc::new(move |val| {
                let own_ctx = Context::new_with_parent(&ctx);

                let mut scope: HashMap<String, Value> = HashMap::new();

                scope.insert(arg.clone(), val);

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

fn eval_apply(f: &ast::Apply, ctx: &Context) -> Result<Value> {
    let apply_code = "<->";

    tracing::debug_span!("eval_apply", code = apply_code).in_scope(|| {
        let fun = f.lambda().ok_or_else(|| eyre!("Missing function"))?;

        let callable = match fun {
            ast::Expr::Ident(ref rf) => {
                let name = rf.ident_token().ok_or_else(|| eyre!("Missing token"))?;
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

fn eval_select(s: &ast::Select, ctx: &Context) -> Result<Value> {
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
                ast::Attr::Ident(i) => i.syntax().text().to_string(),
                ast::Attr::Dynamic(_) => todo!(),
                ast::Attr::Str(_) => todo!(),
            })
            .collect::<Vec<String>>();

        if attrpath.len() > 1 {
            Err(eyre!("Nested-attrpath is not yet implemented"))?;
        }

        let set = s.expr().ok_or_else(|| eyre!("Missing set"))?;

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

fn eval_let_in(li: &ast::LetIn, ctx: &Context) -> Result<Value> {
    let let_in_code = li
        .let_token()
        .and_then(|t| t.parent())
        .map(|t| t.text().to_string())
        .unwrap();

    tracing::debug_span!("eval_let_in", code = let_in_code).in_scope(|| {
        todo!("let-in has to be redone after relevant rnix nodes have been understood")
        // let own_ctx = Context::new_with_parent(ctx);

        // let mut scope: HashMap<String, Value> = HashMap::new();

        // for binding in li.bindings() {
        //     use nil_syntax::ast::Binding;
        //     let (name, value_expr) = match binding {
        //         Binding::Inherit(_) => Err(eyre!("Inherit is not implemented"))?,
        //         Binding::AttrpathValue(path) => (
        //             path.attrpath()
        //                 .ok_or_else(|| eyre!("Missing attrpath for binding"))?
        //                 .attrs()
        //                 .map(|attr| match attr {
        //                     nil_syntax::ast::Attr::Dynamic(_) => todo!(),
        //                     nil_syntax::ast::Attr::Name(name) => {
        //                         name.token().unwrap().text().into()
        //                     }
        //                     nil_syntax::ast::Attr::String(_) => todo!(),
        //                 })
        //                 .collect::<Vec<String>>(),
        //             path.value()
        //                 .ok_or_else(|| eyre!("Missing value for binding"))?,
        //         ),
        //     };

        //     if name.len() > 1 {
        //         Err(eyre!("Nested-attrpath is not yet implemented"))?;
        //     }

        //     // scope[&name[0]] = Value::Thunk(value_expr.clone());
        //     scope.insert(
        //         name[0].clone(),
        //         Value::Thunk(value_expr.clone(), own_ctx.clone()),
        //     );
        // }

        // {
        //     let mut ctx_builder = own_ctx.0.lock();
        //     ctx_builder.values = scope;
        // }

        // li.body()
        //     .map(|body| eval_expr(&body, &own_ctx))
        //     .unwrap_or_else(|| Ok(Value::Bool(false)))
    })
}

fn eval_attr_set(set: &ast::AttrSet, ctx: &Context) -> Result<Value> {
    let attr_set_code = set
        .l_curly_token()
        .and_then(|t| t.parent())
        .map(|t| t.text())
        .unwrap()
        .to_string();

    tracing::debug_span!("eval_attr_set", code = attr_set_code).in_scope(|| {
        todo!("attr-set has to be redone after relevant rnix nodes have been understood")
        // let own_ctx = Context::new_with_parent(ctx);

        // let mut scope: HashMap<String, Value> = HashMap::new();

        // for binding in set.bindings() {
        //     use nil_syntax::ast::Binding;
        //     let (name, value_expr) = match binding {
        //         Binding::Inherit(_) => Err(eyre!("Inherit is not implemented"))?,
        //         Binding::AttrpathValue(path) => (
        //             path.attrpath()
        //                 .ok_or_else(|| eyre!("Missing attrpath for binding"))?
        //                 .attrs()
        //                 .map(|attr| match attr {
        //                     nil_syntax::ast::Attr::Dynamic(_) => todo!(),
        //                     nil_syntax::ast::Attr::Name(name) => {
        //                         name.token().unwrap().text().into()
        //                     }
        //                     nil_syntax::ast::Attr::String(_) => todo!(),
        //                 })
        //                 .collect::<Vec<String>>(),
        //             path.value()
        //                 .ok_or_else(|| eyre!("Missing value for binding"))?,
        //         ),
        //     };

        //     if name.len() > 1 {
        //         Err(eyre!("Nested-attrpath is not yet implemented"))?;
        //     }

        //     // scope[&name[0]] = Value::Thunk(value_expr.clone());
        //     scope.insert(
        //         name[0].clone(),
        //         Value::Thunk(value_expr.clone(), own_ctx.clone()),
        //     );
        // }

        // {
        //     let mut ctx_builder = own_ctx.0.lock();
        //     ctx_builder.values = scope.clone();
        // }

        // Ok(Value::AttrSet(scope))
    })
}

// fn eval_str(s: &ast::StringAst, _ctx: &Context) -> Result<Value> {
//     let string_code = s
//         .start_dquote_token()
//         .and_then(|t| t.parent())
//         .map(|t| t.text())
//         .unwrap()
//         .to_string();

//     tracing::debug_span!("eval_str", code = string_code, string = field::Empty).in_scope(|| {
//         let mut result = String::new();

//         for part in s.string_parts() {
//             match part {
//                 StringPart::Fragment(f) => result.push_str(f.text()),
//                 StringPart::Escape(e) => todo!("Escape {:?}", e),
//                 StringPart::Dynamic(d) => todo!("Dynamic {:?}", d),
//             }

//             Span::current().record("string", &result);
//         }

//         result.shrink_to_fit();

//         Ok(Value::String(result))
//     })
// }

// fn eval_indent_str(is: &IndentString, _ctx: &Context) -> Result<Value> {
//     let string_code = is
//         .start_quote2_token()
//         .and_then(|t| t.parent())
//         .map(|t| t.text())
//         .unwrap()
//         .to_string();

//     let start = is.start_quote2_token().unwrap().text_range().start();
//     let stop = is.end_quote2_token().unwrap().text_range().end();
//     let capacity_guess = stop - start;

//     tracing::debug_span!(
//         "eval_str",
//         code = string_code,
//         string = field::Empty,
//         start = usize::from(start),
//         stop = usize::from(stop),
//         capacity_guess = usize::from(capacity_guess)
//     )
//     .in_scope(|| {
//         let mut result = String::with_capacity(capacity_guess.into());

//         for part in is.string_parts() {
//             match part {
//                 StringPart::Fragment(f) => result.push_str(f.text()),
//                 StringPart::Escape(e) => todo!("Escape {:?}", e),
//                 StringPart::Dynamic(d) => todo!("Dynamic {:?}", d),
//             }

//             Span::current().record("string", &result);
//         }

//         result = textwrap::dedent(&result);

//         Ok(Value::String(result))
//     })
// }

// fn eval_ref(rf: &ast::Ref, ctx: &Context) -> Result<Value> {
//     let ref_code = rf.token().map(|t| t.text().to_string()).unwrap();

//     tracing::debug_span!("eval_ref", code = ref_code).in_scope(|| match ref_code.as_str() {
//         "true" => Ok(Value::Bool(true)),
//         "false" => Ok(Value::Bool(false)),
//         name => Ok(ctx.resolve(name)?),
//     })
// }

fn eval_literal(lit: &ast::Literal, _ctx: &Context) -> Result<Value> {
    let lit_code = lit.syntax().text().to_string();

    tracing::debug_span!("eval_literal", code = lit_code).in_scope(|| match lit.kind() {
        ast::LiteralKind::Integer(i) => Ok(Value::Integer(i.value()?)),
        ast::LiteralKind::Float(f) => Ok(Value::Float(f.value()?)),
        token => Err(eyre!("eval literal: {:?}", token)),
    })
}

fn eval_bin_op(bin_op: &ast::BinOp, ctx: &Context) -> Result<Value> {
    let code = bin_op.syntax().text().to_string();

    tracing::debug_span!("eval_binop", code).in_scope(|| {
        let lhs = eval_expr(&bin_op.lhs().ok_or_else(|| eyre!("Missing LHS"))?, ctx)?;
        let rhs = eval_expr(&bin_op.rhs().ok_or_else(|| eyre!("Missing RHS"))?, ctx)?;
        match (lhs, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => match bin_op.operator().unwrap() {
                ast::BinOpKind::Add => Ok(Value::Integer(lhs + rhs)),
                ast::BinOpKind::Sub => Ok(Value::Integer(lhs - rhs)),
                ast::BinOpKind::Mul => Ok(Value::Integer(lhs * rhs)),
                ast::BinOpKind::Div => Ok(Value::Integer(lhs / rhs)),
                op_token => todo!("op_token: {:?}", op_token),
            },
            (Value::Integer(lhs), Value::Float(rhs)) => match bin_op.operator().unwrap() {
                ast::BinOpKind::Add => Ok(Value::Float(lhs as f64 + rhs)),
                ast::BinOpKind::Sub => Ok(Value::Float(lhs as f64 - rhs)),
                ast::BinOpKind::Mul => Ok(Value::Float(lhs as f64 * rhs)),
                ast::BinOpKind::Div => Ok(Value::Float(lhs as f64 / rhs)),
                op_token => todo!("op_token: {:?}", op_token),
            },
            (Value::Float(lhs), Value::Integer(rhs)) => match bin_op.operator().unwrap() {
                ast::BinOpKind::Add => Ok(Value::Float(lhs + rhs as f64)),
                ast::BinOpKind::Sub => Ok(Value::Float(lhs - rhs as f64)),
                ast::BinOpKind::Mul => Ok(Value::Float(lhs * rhs as f64)),
                ast::BinOpKind::Div => Ok(Value::Float(lhs / rhs as f64)),
                op_token => todo!("op_token: {:?}", op_token),
            },
            (Value::Float(lhs), Value::Float(rhs)) => match bin_op.operator().unwrap() {
                ast::BinOpKind::Add => Ok(Value::Float(lhs + rhs)),
                ast::BinOpKind::Sub => Ok(Value::Float(lhs - rhs)),
                ast::BinOpKind::Mul => Ok(Value::Float(lhs * rhs)),
                ast::BinOpKind::Div => Ok(Value::Float(lhs / rhs)),
                op_token => todo!("op_token: {:?}", op_token),
            },
            (lhs, rhs) => todo!("lhs: {:?}, rhs: {:?}", lhs, rhs),
        }
    })
}

fn eval_unary_op(unary_op: &ast::UnaryOp, ctx: &Context) -> Result<Value> {
    let unary_code = unary_op.syntax().text().to_string();

    tracing::debug_span!("eval_unary_op", code = unary_code).in_scope(|| {
        let rhs = eval_expr(
            &unary_op
                .expr()
                .ok_or_else(|| eyre!("Missing argument for unary operator"))?,
            ctx,
        )?;
        match rhs {
            Value::Integer(val) => match unary_op.operator().unwrap() {
                ast::UnaryOpKind::Negate => Ok(Value::Integer(-val)),
                ast::UnaryOpKind::Invert => Err(eyre!("Not is not implemented")),
            },
            Value::Float(val) => match unary_op.operator().unwrap() {
                ast::UnaryOpKind::Negate => Ok(Value::Float(-val)),
                ast::UnaryOpKind::Invert => Err(eyre!("Not is not implemented")),
            },
            Value::Bool(val) => match unary_op.operator().unwrap() {
                ast::UnaryOpKind::Invert => Ok(Value::Bool(!val)),
                ast::UnaryOpKind::Negate => Err(eyre!("Negate is not implemented")),
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

    // #[rstest]
    // #[case("true", Value::Bool(true))] // TODO
    // #[case("false", Value::Bool(false))] // TODO
    // #[case::negated("!true", Value::Bool(false))] // TODO
    // fn simple_booleans(#[case] code: &str, #[case] expected: Value) {
    //     assert_eq!(super::code(code).unwrap(), expected);
    // }

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
    // #[case("a/b", Value::Path("a/b".into()))] // TODO
    // #[case("./a/b", Value::Path("./a/b".into()))] // TODO
    // #[case("/a/b", Value::Path("/a/b".into()))] // TODO
    // #[case("~/a/b", Value::Path("~/a/b".into()))] // TODO
    // #[case("\"~/a/b\"", Value::String("~/a/b".into()))] // TODO
    // #[case(INDENT_STRING, Value::String("\nfoo\nbar\n".into()))] // TODO
    // #[case("{}", Value::AttrSet(HashMap::new()))] // TODO
    fn simple_literals(#[case] code: &str, #[case] expected: Value) {
        assert_eq!(super::code(code).unwrap(), expected);
    }

    // #[rstest]
    // #[case::let_in("let a = 1; in a", Value::Integer(1))] // TODO
    // #[case::attr_set("{a = 1;}.a", Value::Integer(1))] // TODO
    // #[case::combine("let s = {a = 1;}; in s.a", Value::Integer(1))] // TODO
    // #[case::or("let s = {a = 1;}; in s.b or false", Value::Bool(false))] // TODO
    // #[case::nested_let("let a = let b = 1; in b; in a", Value::Integer(1))] // TODO
    // fn variables(#[case] code: &str, #[case] expected: Value) {
    //     assert_eq!(super::code(code).unwrap(), expected);
    // }

    // #[rstest]
    // #[case::fun_def_id("let id = a: a; in id 1", Value::Integer(1))] // TODO
    // #[case::fun_def_one_arg("let f = a: 1 + a; in f 1", Value::Integer(2))] // TODO
    // // #[case::fun_def_two_arg("let f = a: b: b + a; in f 1 1", Value::Integer(1))]
    // fn functions(#[case] code: &str, #[case] expected: Value) {
    //     assert_eq!(super::code(code).unwrap(), expected);
    // }

    #[rstest]
    #[case::empty("[]", Value::List(Vec::new()))]
    #[case::one_element("[1]", Value::List(vec![Value::Integer(1)]))]
    #[case::two_elements(r#"[1 2.0]"#, Value::List(vec![Value::Integer(1), Value::Float(2.0)]))]
    fn lists(#[case] code: &str, #[case] expected: Value) {
        assert_eq!(super::code(code).unwrap(), expected);
    }
}
