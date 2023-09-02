// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: MIT

mod eval;

fn main() {
    let code = r#"
        1 + 2
    "#;
    let value = eval::code(code);
    println!("value: {:?}", value);
}
