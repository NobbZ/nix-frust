// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use std::env::args;

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
            for error in errors {
                let span = error.span();
                println!("{}-{}: {}", span.start(), span.end(), error);
                // tracing::error!(?err, "got error");
            }
        }
    }
}
