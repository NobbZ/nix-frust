use eyre::{eyre, Result};
use nil_syntax::{
    ast::{
        BinaryOp, BinaryOpKind, Expr, HasStringParts, IndentString, Literal, LiteralKind, Ref,
        String as StringAst, StringPart, UnaryOp, UnaryOpKind,
    },
    parser,
};

#[derive(Debug, PartialEq)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Bool(bool),
    Path(String),
    String(String),
}

pub fn code(code: &str) -> Result<Value> {
    let expr = parser::parse_file(code)
        .root()
        .expr()
        .ok_or_else(|| eyre!("No root-expression"))?;

    Ok(eval_expr(&expr)?)
}

fn eval_expr(expr: &Expr) -> Result<Value> {
    match expr {
        Expr::Literal(lit) => eval_literal(lit),
        Expr::BinaryOp(bo) => eval_binop(bo),
        Expr::UnaryOp(uo) => eval_unary_op(uo),
        Expr::Ref(rf) => eval_ref(rf),
        Expr::String(s) => eval_str(s),
        Expr::IndentString(is) => eval_indent_str(is),
        expr => Err(eyre!("expr: {:?}", expr)),
    }
}

fn eval_str(s: &StringAst) -> Result<Value> {
    let mut result = String::new();

    for part in s.string_parts() {
        match part {
            StringPart::Fragment(f) => result.push_str(f.text()),
            StringPart::Escape(e) => todo!("Escape {:?}", e),
            StringPart::Dynamic(d) => todo!("Dynamic {:?}", d),
        }
    }

    result.shrink_to_fit();

    Ok(Value::String(result))
}

fn eval_indent_str(is: &IndentString) -> Result<Value> {
    let start = is.start_quote2_token().unwrap().text_range().start();
    let stop = is.end_quote2_token().unwrap().text_range().end();
    let capacity_guess = stop - start;

    let mut result = String::with_capacity(capacity_guess.into());

    for part in is.string_parts() {
        match part {
            StringPart::Fragment(f) => result.push_str(f.text()),
            StringPart::Escape(e) => todo!("Escape {:?}", e),
            StringPart::Dynamic(d) => todo!("Dynamic {:?}", d),
        }
    }

    result = textwrap::dedent(&result);

    Ok(Value::String(result))
}

fn eval_ref(rf: &Ref) -> Result<Value> {
    let token = rf.token().ok_or_else(|| eyre!("Missing token"))?;
    match token.text() {
        "true" => Ok(Value::Bool(true)),
        "false" => Ok(Value::Bool(false)),
        name => Err(eyre!("Unknown name: {:?}", name)),
    }
}

fn eval_literal(lit: &Literal) -> Result<Value> {
    match lit.kind().unwrap() {
        LiteralKind::Int => Ok(Value::Integer(lit.token().unwrap().text().parse::<i64>()?)),
        LiteralKind::Float => Ok(Value::Float(lit.token().unwrap().text().parse::<f64>()?)),
        LiteralKind::Path => Ok(Value::Path(lit.token().unwrap().text().into())),
        token => Err(eyre!("eval literal: {:?}", token)),
    }
}

fn eval_binop(bin_op: &BinaryOp) -> Result<Value> {
    let lhs = eval_expr(&bin_op.lhs().ok_or_else(|| eyre!("Missing LHS"))?)?;
    let rhs = eval_expr(&bin_op.rhs().ok_or_else(|| eyre!("Missing RHS"))?)?;
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
}

fn eval_unary_op(unary_op: &UnaryOp) -> Result<Value> {
    let rhs = eval_expr(
        &unary_op
            .arg()
            .ok_or_else(|| eyre!("Missing argument for unary operator"))?,
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
    }
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
    fn simple_literals(#[case] code: &str, #[case] expected: Value) {
        assert_eq!(super::code(code).unwrap(), expected);
    }
}
