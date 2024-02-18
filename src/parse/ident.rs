// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use chumsky::prelude::*;

use super::Expr;

#[derive(Clone, PartialEq, Debug)]
pub struct Ident(pub String);

impl<S> From<S> for Ident
where
    S: Into<String>,
{
    fn from(s: S) -> Self {
        Self(s.into())
    }
}

impl ToString for Ident {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

pub fn ident<'a>() -> impl Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> + Clone {
    raw_ident().map(Expr::Ident).padded()
}

pub(crate) fn raw_ident<'a>() -> impl Parser<'a, &'a str, Ident, extra::Err<Rich<'a, char>>> + Clone
{
    text::ascii::ident().map(|s: &str| s.into()).padded()
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    #[rstest]
    #[case("a", Expr::Ident("a".into()))]
    #[case("a1", Expr::Ident("a1".into()))]
    fn ident_parser(#[case] code: &str, #[case] expected: Expr) {
        let result = ident().parse(code).unwrap();
        assert_eq!(result, expected);
    }
}
