// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use chumsky::prelude::*;

use super::Expr;

pub fn sum<'a, E>(expr: E) -> impl Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> + Clone
where
    E: Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> + Clone,
{
    product(expr.clone()).foldl(
        choice((
            just('+').padded().to(Expr::Add as fn(_, _) -> _),
            just('-').padded().to(Expr::Sub as fn(_, _) -> _),
        ))
        .then(product(expr))
        .repeated(),
        |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
    )
}

fn product<'a, E>(expr: E) -> impl Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> + Clone
where
    E: Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> + Clone,
{
    expr.clone().foldl(
        choice((
            just('*').padded().to(Expr::Mul as fn(_, _) -> _),
            just('/').padded().to(Expr::Div as fn(_, _) -> _),
        ))
        .then(expr)
        .repeated(),
        |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    use crate::parse::attr_set::{Segment, SetEntry};

    #[rstest]
    #[case::add("1 + 1", Expr::Add(Box::new(Expr::Int(1)), Box::new(Expr::Int(1))))]
    #[case::sub("1 - 1", Expr::Sub(Box::new(Expr::Int(1)), Box::new(Expr::Int(1))))]
    #[case::mul("1 * 1", Expr::Mul(Box::new(Expr::Int(1)), Box::new(Expr::Int(1))))]
    #[case::div("1 / 1", Expr::Div(Box::new(Expr::Int(1)), Box::new(Expr::Int(1))))]
    fn arith(#[case] code: &str, #[case] expected: Expr) {
        let result = sum(super::super::parser()).parse(code).unwrap();
        dbg!(result.clone());
        assert_eq!(result, expected);
    }
}
