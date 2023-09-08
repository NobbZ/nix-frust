// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

use std::env::args;

use tracing::Level;
use tracing_subscriber::{fmt::format::FmtSpan, FmtSubscriber};

use crate::compiler::Compiler;

mod compiler;
mod eval;
mod upvalues;
mod values;

fn main() {
    FmtSubscriber::builder()
        .with_span_events(FmtSpan::NEW | FmtSpan::CLOSE)
        .with_max_level(Level::DEBUG)
        .init();

    let code = args().nth(1).unwrap();

    // let value = eval::code(&code);
    let mut compiler = Compiler::new(&code, None);
    let root_span = compiler.to_span(compiler.expr);
    let root_slot = compiler.scope_mut().declare_phantom(false);
    compiler.compile(compiler.expr.clone(), root_slot);
    // tracing::info!(?value, "got result");
    dbg!(&compiler);

    tracing::debug!(warnings = ?compiler.warnings, "the warnings");
    tracing::debug!(errors = ?compiler.errors, "the errors");

    tracing::info!(?compiler, "got result");
}
