// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use std::{env::args, ops::Range};

use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
use chumsky::prelude::*;
use tracing::Level;
use tracing_subscriber::{fmt::format::FmtSpan, FmtSubscriber};

mod extensions;
mod parse;

fn main() {
    FmtSubscriber::builder()
        .with_span_events(FmtSpan::NEW | FmtSpan::CLOSE)
        .with_max_level(Level::DEBUG)
        .init();

    let code = args().nth(1).unwrap();

    let ast = parse::parser().parse(&code).into_result();

    match ast {
        Ok(ast) => {
            tracing::info!(?ast, "got result");
        }
        Err(errors) => {
            let mut colors = ColorGenerator::new();

            for error in errors {
                let span = error.span();
                Report::<Range<usize>>::build(ReportKind::Error, (), 0)
                    .with_message("syntax error")
                    .with_label(
                        Label::new(span.into_range())
                            .with_message(error.to_string())
                            .with_color(colors.next()),
                    )
                    .finish()
                    .print(Source::from(&code))
                    .unwrap();
                // let span = error.span();
                println!("{}-{}: {}", span.start(), span.end(), error);
                // tracing::error!(?err, "got error");
            }
        }
    }
}
