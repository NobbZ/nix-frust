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

pub fn parser<'a>() -> impl Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> {
    recursive(|expr| {
        choice((float::float(), integer::integer())).or(expr
            .delimited_by(just('('), just(')'))
            .map(|expr| Expr::Parens(Box::new(expr))))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    #[rstest]
    #[case::post_int("1", Expr::Int(1))]
    #[case("1.0", Expr::Flt(1.0))]
    #[case::neg_int("-1", Expr::Neg(Box::new(Expr::Int(1))))]
    #[case("-1.0", Expr::Flt(-1.0))]
    #[case::par_int("(1)", Expr::Parens(Box::new(Expr::Int(1))))]
    #[case("(1.1)", Expr::Parens(Box::new(Expr::Flt(1.1))))]
    fn numbers(#[case] code: &str, #[case] expected: Expr) {
        let parse_result = parser().parse(code);
        let expr = parse_result.output();
        let errors: Vec<_> = parse_result.errors().collect();
        dbg!(&errors);
        assert_eq!(expr, Some(&expected));
    }
}
