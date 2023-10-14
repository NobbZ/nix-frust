// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use std::ops::Range;

use chumsky::prelude::*;

use super::Expr;
use crate::extensions::string::StringExt;

#[derive(Debug, PartialEq)]
enum Sign {
    Neg,
    Pos,
}

#[derive(Debug, PartialEq)]
struct Flt(Option<Sign>, String, String, Option<(Option<Sign>, String)>);

// TODO: This clippy lint should be fixed eventually
#[allow(clippy::result_large_err)]
fn convert_to_expr(f: Flt, span: Range<usize>) -> Result<Expr, Simple<char>> {
    let s = match f.0 {
        Some(Sign::Neg) => "-",
        _ => "",
    };

    let pre = f.1.non_empty_or("0");
    let post = f.2.non_empty_or("0");

    let e = f.3.map_or_else(
        || "".to_string(),
        |exp| match exp {
            (Some(Sign::Neg), e) => format!("e-{}", e),
            (_, e) => format!("e+{}", e),
        },
    );

    let f = format!(
        "{sign}{pre}.{post}{exponent}",
        sign = s,
        pre = pre,
        post = post,
        exponent = e
    );

    match f.parse::<f64>() {
        Ok(n) => Ok(Expr::Flt(n)),
        Err(_) => Err(Simple::custom(span, format!("Invalid float: {}", f))),
    }
}

fn sign() -> impl Parser<char, Sign, Error = Simple<char>> + Clone {
    one_of("+-").map(|s| match s {
        '+' => Sign::Pos,
        '-' => Sign::Neg,
        _ => unreachable!(),
    })
}

fn marker() -> impl Parser<char, char, Error = Simple<char>> + Clone {
    one_of("eE")
}

fn digits(min_length: usize) -> impl Parser<char, String, Error = Simple<char>> + Clone {
    one_of("1234567890")
        .repeated()
        .at_least(min_length)
        .map(|s| s.into_iter().collect())
}

fn exponent() -> impl Parser<char, (Option<Sign>, String), Error = Simple<char>> + Clone {
    marker().then(sign().or_not()).then(digits(0)).map(|e| {
        let ((_, sign), digits) = e;
        (sign, digits)
    })
}

fn variant_one() -> impl Parser<char, Flt, Error = Simple<char>> + Clone {
    sign()
        .or_not()
        .then(digits(0))
        .then(just('.'))
        .then(digits(0))
        .then(exponent().or_not())
        .map(|x| match x {
            ((((sign, pre), '.'), post), exp) => Flt(sign, pre, post, exp),
            _ => unreachable!(),
        })
}

fn variant_two() -> impl Parser<char, Flt, Error = Simple<char>> + Clone {
    sign()
        .or_not()
        .then(digits(1))
        .then((just('.').then(digits(0))).or_not())
        .then(exponent())
        .map(|x| match x {
            (((sign, pre), Some((_, post))), exp) => Flt(sign, pre, post, Some(exp)),
            (((sign, pre), None), exp) => Flt(sign, pre, "".to_string(), Some(exp)),
        })
}

pub fn float() -> impl Parser<char, Expr, Error = Simple<char>> + Clone {
    variant_one()
        .or(variant_two())
        .try_map(convert_to_expr)
        .padded()
        .then_ignore(end())
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    #[rstest]
    #[case::dot("0.0", Expr::Flt(0.0))]
    #[case::positive("1.", Expr::Flt(1.0))]
    #[case::negative("-1.", Expr::Flt(-1.0))]
    #[case::zero("0.0", Expr::Flt(0.0))]
    #[case::alt_zero("0e0", Expr::Flt(0.0))]
    #[case::minus_zero("-.0", Expr::Flt(0.0))]
    #[case::pi("3.141592653589793e0", Expr::Flt(3.141592653589793e0))]
    #[case::log_10_2("3.010299957e-1", Expr::Flt(3.010299957e-1))]
    fn numbers(#[case] code: &str, #[case] expected: Expr) {
        let result = float().parse(code).unwrap();
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case::integer("1")]
    fn errors(#[case] code: &str) {
        let result = float().parse(code);
        assert!(result.is_err());
    }
}
