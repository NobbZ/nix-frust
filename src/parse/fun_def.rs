// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use chumsky::prelude::*;

use super::{
    ident::{self, Ident},
    Expr,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Ident(Ident),
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunHead {
    Ident(Ident),
    Pattern(Vec<Pattern>),
    AtPattern(Vec<Pattern>, Ident),
    PatternAt(Vec<Pattern>, Ident),
}

pub fn fun_def<'a, E>(expr: E) -> impl Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> + Clone
where
    E: Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> + Clone,
{
    choice((
        at_pattern(),
        pattern_at(),
        pattern(),
        ident::raw_ident().map(FunHead::Ident),
    ))
    .then_ignore(just(':'))
    .then(expr)
    .map(|(head, body)| Expr::FunDef(head, Box::new(body)))
}

fn pattern<'a>() -> impl Parser<'a, &'a str, FunHead, extra::Err<Rich<'a, char>>> + Clone {
    ident::raw_ident()
        .map(Pattern::Ident)
        .separated_by(just(',').padded())
        .collect()
        .delimited_by(just('{'), just('}'))
        .map(FunHead::Pattern)
}

fn at_pattern<'a>() -> impl Parser<'a, &'a str, FunHead, extra::Err<Rich<'a, char>>> + Clone {
    ident::raw_ident()
        .then_ignore(just('@').padded())
        .then(pattern())
        .map(|(name, pattern)| match pattern {
            FunHead::Pattern(pattern) => FunHead::AtPattern(pattern, name),
            _ => unreachable!(),
        })
}

fn pattern_at<'a>() -> impl Parser<'a, &'a str, FunHead, extra::Err<Rich<'a, char>>> + Clone {
    pattern()
        .then_ignore(just('@').padded())
        .then(ident::raw_ident())
        .map(|(pattern, name)| match pattern {
            FunHead::Pattern(pattern) => FunHead::PatternAt(pattern, name),
            _ => unreachable!(),
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    #[rstest]
    #[case::id("x: x", Expr::FunDef(FunHead::Ident(Ident("x".into())), Box::new(Expr::Ident(Ident("x".into())))))]
    #[case::empty_pattern("{}: 1", Expr::FunDef(FunHead::Pattern(vec![]), Box::new(Expr::Int(1))))]
    #[case::one_ident_pattern("{x}: 1", Expr::FunDef(FunHead::Pattern(vec![Pattern::Ident("x".into())]), Box::new(Expr::Int(1))))]
    #[case::two_ident_pattern("{x, y}: 1", Expr::FunDef(FunHead::Pattern(vec![Pattern::Ident("x".into()), Pattern::Ident("y".into())]), Box::new(Expr::Int(1))))]
    #[case::at_pattern("a @ {}: 1", Expr::FunDef(FunHead::AtPattern(vec![], "a".into()), Box::new(Expr::Int(1))))]
    #[case::at_pattern("{} @ a: 1", Expr::FunDef(FunHead::PatternAt(vec![], "a".into()), Box::new(Expr::Int(1))))]
    fn fun_def_parser(#[case] code: &str, #[case] expected: Expr) {
        let result = fun_def(super::super::parser()).parse(code).unwrap();
        assert_eq!(result, expected);
    }
}
