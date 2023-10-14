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
    List(Vec<Self>),
}

pub fn parser<'a>() -> impl Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> {
    recursive(|expr| {
        choice((
            choice((float::float(), integer::integer())),
            expr.clone()
                .delimited_by(just('('), just(')'))
                .map(|expr| Expr::Parens(Box::new(expr))),
            expr.repeated()
                .collect()
                .delimited_by(just('['), just(']'))
                .map(Expr::List),
        ))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    macro_rules! int {
        ($n:expr) => {
            Expr::Int($n)
        };
    }

    macro_rules! flt {
        ($n:expr) => {
            Expr::Flt($n)
        };
    }

    macro_rules! neg {
        ($e:expr) => {
            Expr::Neg(Box::new($e))
        };
    }

    macro_rules! parens {
        ($e:expr) => {
            Expr::Parens(Box::new($e))
        };
    }

    macro_rules! list {
        ($($e:expr),*) => {
            Expr::List(vec![$($e),*])
        };
    }

    #[rstest]
    #[case::pos_int("1", int!(1))]
    #[case::pos_flt("1.0", flt!(1.0))]
    #[case::neg_int("-1", neg!(int!(1)))]
    #[case::neg_flt("-1.0", flt!(-1.0))]
    #[case::par_int("(1)", parens!(int!(1)))]
    #[case::par_neg_int("(-11)", parens!(neg!(int!(11))))]
    #[case::par_flt("(1.1)", parens!(flt!(1.1)))]
    #[case::list_empty("[]", list!())]
    #[case::list_one("[1]", list!(int!(1)))]
    #[case::list_two("[1 2]", list!(int!(1), int!(2)))]
    #[case::list_het("[1 2.1]", list!(int!(1), flt!(2.1)))]
    fn numbers(#[case] code: &str, #[case] expected: Expr) {
        let parse_result = parser().parse(code);
        let expr = parse_result.output();
        let errors: Vec<_> = parse_result.errors().collect();
        dbg!(&errors);
        assert_eq!(expr, Some(&expected));
    }
}
