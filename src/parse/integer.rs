// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use chumsky::prelude::*;

use super::Expr;

pub fn integer() -> impl Parser<char, Expr, Error = Simple<char>> + Clone {
    let int = text::int(10)
        .map(|s: String| Expr::Int(s.parse().unwrap()))
        .padded();

    let op = |c| just(c).padded();

    let unary = op('-')
        .repeated()
        .then(int)
        .foldr(|_op, rhs| Expr::Neg(Box::new(rhs)));

    unary.then_ignore(end())
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    #[rstest]
    #[case::positive("1", Expr::Int(1))]
    #[case::negative("-1", Expr::Neg(Box::new(Expr::Int(1))))]
    fn numbers(#[case] code: &str, #[case] expected: Expr) {
        let result = integer().parse(code).unwrap();
        assert_eq!(result, expected);
    }
}
