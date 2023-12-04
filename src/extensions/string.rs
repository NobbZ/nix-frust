// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: GPL-3.0-or-later

pub(crate) trait StringExt {
    fn non_empty_or<S>(self, default: S) -> Self
    where
        S: AsRef<str>;
}

impl StringExt for String {
    fn non_empty_or<S>(self, default: S) -> Self
    where
        S: AsRef<str>,
    {
        if self.is_empty() {
            default.as_ref().to_string()
        } else {
            self
        }
    }
}
