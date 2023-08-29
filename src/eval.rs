use eyre::{eyre, Result};
use nil_syntax::{ast::Expr, parser};

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
        Expr::Literal(lit) => match lit.kind().unwrap() {
            nil_syntax::ast::LiteralKind::Int => {
                Ok(Value::Integer(lit.token().unwrap().text().parse::<i64>()?))
            }

            token => todo!("{:?}", token),
        },
        Expr::BinaryOp(bo) => {
            let lhs = eval_expr(&bo.lhs().unwrap())?;
            let rhs = eval_expr(&bo.rhs().unwrap())?;
            match (lhs, rhs) {
                (Value::Integer(lhs), Value::Integer(rhs)) => match bo.op_kind().unwrap() {
                    nil_syntax::ast::BinaryOpKind::Add => Ok(Value::Integer(lhs + rhs)),
                    op_token => todo!("op_token: {:?}", op_token),
                },
            }
        }
        Expr::UnaryOp(uo) => {
            let rhs = eval_expr(
                &uo.arg()
                    .ok_or_else(|| eyre!("Missing argument for unary operator"))?,
            )?;
            match rhs {
                Value::Integer(rhs) => match uo.op_kind().unwrap() {
                    nil_syntax::ast::UnaryOpKind::Negate => Ok(Value::Integer(-rhs)),
                    nil_syntax::ast::UnaryOpKind::Not => todo!("Not is not implemented"),
                },
            }
        }
        expr => todo!("expr: {:?}", expr),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
