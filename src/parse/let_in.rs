// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use chumsky::prelude::*;

use super::{
    ident::{self, Ident},
    Expr,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Binding {
    NameVal(Ident, Expr),
    Inherit(Vec<Ident>),
    InheritFrom(Expr, Vec<Ident>),
}

pub fn let_in<'a, E>(expr: E) -> impl Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> + Clone
where
    E: Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> + Clone,
{
    text::ascii::keyword("let")
        .padded()
        .ignore_then(
            choice((
                inherit_from(expr.clone()),
                inherit_(),
                name_val(expr.clone()),
            ))
            .padded()
            .then_ignore(just(';').padded())
            .repeated()
            .collect(),
        )
        .then_ignore(text::ascii::keyword("in").padded())
        .then(expr)
        .map(|(bindings, expr)| Expr::LetIn(bindings, Box::new(expr)))
}

fn name_val<'a, E>(expr: E) -> impl Parser<'a, &'a str, Binding, extra::Err<Rich<'a, char>>> + Clone
where
    E: Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> + Clone,
{
    ident::raw_ident()
        .then_ignore(just('=').padded())
        .then(expr)
        .map(|(id, expr)| Binding::NameVal(id, expr))
}

fn inherit_<'a>() -> impl Parser<'a, &'a str, Binding, extra::Err<Rich<'a, char>>> + Clone {
    text::ascii::keyword("inherit")
        .padded()
        .ignore_then(ident::raw_ident().repeated().at_least(1).collect())
        .map(Binding::Inherit)
}

fn inherit_from<'a, E>(
    expr: E,
) -> impl Parser<'a, &'a str, Binding, extra::Err<Rich<'a, char>>> + Clone
where
    E: Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> + Clone,
{
    text::ascii::keyword("inherit")
        .padded()
        .ignore_then(expr.delimited_by(just('('), just(')')))
        .then(ident::raw_ident().padded().repeated().at_least(1).collect())
        .map(|(set, attrs): (_, Vec<Ident>)| Binding::InheritFrom(set, attrs))
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    use crate::parse::attr_set::{Segment, SetEntry};

    #[rstest]
    #[case::one_kv("let x=1; in x", Expr::LetIn(vec![Binding::NameVal(Ident("x".into()), Expr::Int(1))], Box::new(Expr::Ident(Ident("x".into())))))]
    #[case::nested_kv("let x= {y=1;}; in x", Expr::LetIn(vec![Binding::NameVal("x".into(), Expr::AttrSet(vec![SetEntry::KeyVal(vec![Segment::Literal("y".into())], Expr::Int(1))]))], Box::new(Expr::Ident("x".into()))))]
    #[case::inherit("let inherit x; in x", Expr::LetIn(vec![Binding::Inherit(vec!["x".into()])], Box::new(Expr::Ident("x".into()))))]
    #[case::inherit_from("let inherit ({}) x; in x", Expr::LetIn(vec![Binding::InheritFrom(Expr::AttrSet(vec![]),vec!["x".into()])], Box::new(Expr::Ident("x".into()))))]
    fn set(#[case] code: &str, #[case] expected: Expr) {
        let result = let_in(super::super::parser()).parse(code).unwrap();
        dbg!(result.clone());
        assert_eq!(result, expected);
    }
}
