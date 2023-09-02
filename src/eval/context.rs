// SPDX-FileCopyrightText: 2023 Norbert Melzer <timmelzer@gmailcom>
//
// SPDX-License-Identifier: MIT

use std::{collections::HashMap, sync::Arc};

use eyre::{eyre, Result};
use parking_lot::Mutex;

use crate::eval::value::Value;

#[derive(Clone, Default, Debug)]
pub struct Context(pub(crate) Arc<Mutex<InnerContext>>);

#[derive(Default, Debug)]
pub struct InnerContext {
    pub(crate) values: HashMap<String, Value>,
    #[allow(dead_code)]
    pub(crate) parent: Option<Context>,
}

impl Context {
    pub(crate) fn new_with_parent(parent: &Self) -> Self {
        Self(Arc::new(Mutex::new(InnerContext {
            values: HashMap::new(),
            parent: Some(parent.clone()),
        })))
    }

    pub(crate) fn resolve<N>(&self, name: N) -> Result<Value>
    where
        N: Into<String>,
    {
        let name = name.into();
        let mut ctx = self.0.lock();

        if let Some(value) = ctx.values.get(&name) {
            let v = match value {
                Value::Thunk(expr, _) => super::eval_expr(expr, self)?,
                value => value.clone(),
            };

            ctx.values.insert(name.clone(), v.clone());

            return Ok(v);
        }

        Err(eyre!("Unknown variable: {}", name))
    }
}
