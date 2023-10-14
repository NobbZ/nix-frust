// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use chumsky::prelude::*;

mod float;
mod integer;

#[derive(Clone, PartialEq, Debug)]
pub enum Expr {
    Int(i64),
    Flt(f64),
    Neg(Box<Self>),
    Parens(Box<Self>),
}

pub fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    let expr = integer::integer().or(float::float()).padded();

    (expr
        .clone()
        .delimited_by(just('('), just(')'))
        .map(|e| Expr::Parens(Box::new(e))))
    .or(expr)
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    #[rstest]
    #[case("1", Expr::Int(1))]
    #[case("1.0", Expr::Flt(1.0))]
    #[case("-1", Expr::Neg(Box::new(Expr::Int(1))))]
    #[case("-1.0", Expr::Flt(-1.0))]
    #[case("(1)", Expr::Parens(Box::new(Expr::Int(1))))]
    #[case("(1.1)", Expr::Parens(Box::new(Expr::Flt(0.1))))]
    fn numbers(#[case] code: &str, #[case] expected: Expr) {
        let (expr, errors) = parser().parse_recovery_verbose(code);
        dbg!(&errors);
        assert_eq!(expr, Some(expected));
    }
}
