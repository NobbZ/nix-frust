// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use chumsky::prelude::*;

use super::Expr;

pub fn integer<'a>() -> impl Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> + Clone {
    let int = text::int::<'a, &'a str, _, _>(10)
        .map(|s| Expr::Int(s.parse().unwrap()))
        .padded();

    let unary = just('-')
        .repeated()
        .collect::<Vec<_>>()
        .then(int)
        .map(|(negs, int)| negs.iter().fold(int, |expr, _| Expr::Neg(Box::new(expr))));

    unary
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
