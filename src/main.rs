// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use std::env::args;

use tracing::Level;
use tracing_subscriber::{fmt::format::FmtSpan, FmtSubscriber};

// mod compiler;
mod eval;

fn main() {
    FmtSubscriber::builder()
        .with_span_events(FmtSpan::NEW | FmtSpan::CLOSE)
        .with_max_level(Level::DEBUG)
        .init();

    let code = args().nth(1).unwrap();

    let value = eval::code(&code);
    tracing::info!(?value, "got result");
}
