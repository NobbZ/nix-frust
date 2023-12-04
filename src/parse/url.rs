// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use chumsky::prelude::*;

use super::Expr;

pub fn url<'a>() -> impl Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> + Clone {
    let schema = one_of(('a'..='z').chain('A'..='Z').collect::<Vec<_>>())
        .repeated()
        .collect::<String>();

    let rest = one_of(
        ('a'..='z')
            .chain('A'..='Z')
            .chain('0'..='9')
            .chain(".-/~+_:".chars())
            .collect::<Vec<_>>(),
    )
    .repeated()
    .collect::<String>();

    group((schema, just(':'), rest))
        .map(|(schema, _, rest)| Expr::Url(format!("{}:{}", schema, rest)))
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    #[rstest]
    #[case("x:x")]
    #[case("https://svn.cs.uu.nl:12443/repos/trace/trunk")]
    #[case("http://www2.mplayerhq.hu/MPlayer/releases/fonts/font-arial-iso-8859-1.tar.bz2")]
    #[case("http://losser.st-lab.cs.uu.nl/~armijn/.nix/gcc-3.3.4-static-nix.tar.gz")]
    #[case("http://fpdownload.macromedia.com/get/shockwave/flash/english/linux/7.0r25/install_flash_player_7_linux.tar.gz")]
    #[case(
        "https://ftp5.gwdg.de/pub/linux/archlinux/extra/os/x86_64/unzip-6.0-14-x86_64.pkg.tar.zst"
    )]
    #[case("ftp://ftp.gtk.org/pub/gtk/v1.2/gtk+-1.2.10.tar.gz")]
    fn urls(#[case] code: &str) {
        let result = url().parse(code).unwrap();
        assert_eq!(result, Expr::Url(code.to_owned()));
    }

    #[rstest]
    #[case::brackets("[x:x]")]
    fn errors(#[case] code: &str) {
        let result = url().parse(code).into_result();
        dbg!(&result);
        assert!(result.is_err());
    }
}
