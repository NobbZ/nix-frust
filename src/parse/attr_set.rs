// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use chumsky::prelude::*;

use super::Expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Segment {
    Literal(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SetEntry {
    KeyVal(Vec<Segment>, Expr),
    // TODO: Use some "identifier" type here
    Inherit(Vec<String>),
    InheritFrom(Expr, Vec<String>),
}

pub fn attr_set<'a, E>(
    expr: E,
) -> impl Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> + Clone
where
    E: Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> + Clone,
{
    choice((key_val(expr.clone()), inherit_from(expr), inherit_()))
        .padded()
        .then_ignore(just(';').padded())
        .repeated()
        .collect()
        .map(Expr::AttrSet)
        .delimited_by(just('{'), just('}'))
}

fn key_val<'a, E>(expr: E) -> impl Parser<'a, &'a str, SetEntry, extra::Err<Rich<'a, char>>> + Clone
where
    E: Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> + Clone,
{
    segment()
        .separated_by(just('.'))
        .at_least(1)
        .collect()
        .then(just('=').padded())
        .then(expr)
        .map(|((segments, _), expr)| SetEntry::KeyVal(segments, expr))
}

fn segment<'a>() -> impl Parser<'a, &'a str, Segment, extra::Err<Rich<'a, char>>> + Clone {
    text::ascii::ident().map(|i: &str| Segment::Literal(i.to_string()))
}

fn inherit_<'a>() -> impl Parser<'a, &'a str, SetEntry, extra::Err<Rich<'a, char>>> + Clone {
    text::ascii::keyword("inherit")
        .padded()
        .ignore_then(
            text::ascii::ident()
                .padded()
                .repeated()
                .at_least(1)
                .collect(),
        )
        .map(|attrs: Vec<&str>| SetEntry::Inherit(attrs.iter().map(|s| s.to_string()).collect()))
}

fn inherit_from<'a, E>(
    expr: E,
) -> impl Parser<'a, &'a str, SetEntry, extra::Err<Rich<'a, char>>> + Clone
where
    E: Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> + Clone,
{
    text::ascii::keyword("inherit")
        .padded()
        .ignore_then(expr.delimited_by(just('('), just(')')))
        .then(
            text::ascii::ident()
                .padded()
                .repeated()
                .at_least(1)
                .collect(),
        )
        .map(|(set, attrs): (_, Vec<&str>)| {
            SetEntry::InheritFrom(set, attrs.iter().map(|s| s.to_string()).collect())
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    #[rstest]
    #[case::empty("{}", vec![])]
    #[case::one_kv("{x=1;}", vec![SetEntry::KeyVal(vec![Segment::Literal("x".to_string())], Expr::Int(1))])]
    #[case::nested_kv("{x= {y=1;};}", vec![SetEntry::KeyVal(vec![Segment::Literal("x".to_string())], Expr::AttrSet(vec![SetEntry::KeyVal(vec![Segment::Literal("y".to_string())], Expr::Int(1))]))])]
    #[case::inherit("{inherit x;}", vec![SetEntry::Inherit(vec!["x".to_string()])])]
    #[case::inherit_from("{inherit ({}) x;}", vec![SetEntry::InheritFrom(Expr::AttrSet(vec![]),vec!["x".to_string()])])]
    fn set(#[case] code: &str, #[case] expected: Vec<SetEntry>) {
        let result = attr_set(super::super::parser()).parse(code).unwrap();
        assert_eq!(result, Expr::AttrSet(expected));
    }

    #[rstest]
    #[case::empty_inherit("{inherit;}")]
    #[case::empty_inherit_from("{inherit () a;}")]
    #[case::empty_inherit_from_2("{inherit ({});}")]
    fn parse_errors(#[case] code: &str) {
        let result = attr_set(super::super::parser()).parse(code).into_result();
        dbg!(&result);
        assert!(result.is_err());
    }
}
