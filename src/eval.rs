use eyre::{eyre, Result};
use nil_syntax::{
    ast::{BinaryOp, BinaryOpKind, Expr, Literal, LiteralKind, UnaryOp, UnaryOpKind},
    parser,
};

#[derive(Debug, PartialEq, Eq)]
pub enum Value {
    Integer(i64),
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
        expr => Err(eyre!("expr: {:?}", expr)),
    }
}

fn eval_literal(lit: &Literal) -> Result<Value> {
    match lit.kind().unwrap() {
        LiteralKind::Int => Ok(Value::Integer(lit.token().unwrap().text().parse::<i64>()?)),
        token => Err(eyre!("{:?}", token)),
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
    }
}

fn eval_unary_op(unary_op: &UnaryOp) -> Result<Value> {
    let rhs = eval_expr(
        &unary_op
            .arg()
            .ok_or_else(|| eyre!("Missing argument for unary operator"))?,
    )?;
    match rhs {
        Value::Integer(rhs) => match unary_op.op_kind().unwrap() {
            UnaryOpKind::Negate => Ok(Value::Integer(-rhs)),
            UnaryOpKind::Not => Err(eyre!("Not is not implemented")),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    #[rstest]
    #[case::two_summands("1 + 2", Value::Integer(3))]
    #[case::three_summands("1 + 2 + 3", Value::Integer(6))]
    #[case::two_no_space("1+2", Value::Integer(3))]
    #[case::three_no_space("1+2+3", Value::Integer(6))]
    #[case::lhs_neg("-1 + 2", Value::Integer(1))]
    #[case::rhs_neg("1 + -2", Value::Integer(-1))]
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
    fn simple_multiplication(#[case] code: &str, #[case] expected: Value) {
        assert_eq!(super::code(code).unwrap(), expected);
    }

    #[rstest]
    #[case::two_summands("4 / 2", Value::Integer(2))]
    #[case::three_summands("12 / 2 / 3", Value::Integer(2))]
    #[case::lhs_neg("-4 / 2", Value::Integer(-2))]
    #[case::rhs_neg("4 / -2", Value::Integer(-2))]
    // Regular nix would parse this as a path, not a division.
    // #[case::two_no_space("4/2", Value::Integer(2))]
    // #[case::three_no_space("12/2/3", Value::Integer(2))]
    fn simple_division(#[case] code: &str, #[case] expected: Value) {
        assert_eq!(super::code(code).unwrap(), expected);
    }
}
