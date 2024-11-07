use koopa::ir::{builder::LocalInstBuilder, Function, Program, Value};
use std::collections::HashMap;

use super::{Error, Result};

#[derive(Clone, Copy)]
pub enum SymbolValue {
    Variable(Value),
    Const(Value),
}

impl SymbolValue {
    pub fn into_value(self, program: &mut Program, scope: &mut Scope) -> Value {
        match self {
            SymbolValue::Variable(value) => {
                let v = program
                    .func_mut(scope.function.unwrap())
                    .dfg_mut()
                    .new_value()
                    .load(value);
                scope.instructions.push(v);
                v
            }
            SymbolValue::Const(value) => value,
        }
    }
}

pub(crate) struct Scope<'ast> {
    pub(crate) function: Option<Function>,
    pub(crate) instructions: Vec<Value>,
    pub(crate) symbol_table: HashMap<&'ast str, SymbolValue>,
}

impl<'ast> Scope<'ast> {
    pub(crate) fn new(
        function: Option<Function>,
        instructions: Vec<Value>,
        symbol_table: HashMap<&'ast str, SymbolValue>,
    ) -> Self {
        Self {
            function,
            instructions,
            symbol_table,
        }
    }

    pub fn add(&mut self, k: &'ast str, v: SymbolValue) -> Result<()> {
        if let Some(old) = self.symbol_table.get(k) {
            if let SymbolValue::Const(_) = old {
                return Err(Error::ReassignConst);
            }
        }
        self.symbol_table.insert(k, v);
        Ok(())
    }

    pub fn get(&self, k: &'ast str) -> Result<SymbolValue> {
        match self.symbol_table.get(k) {
            Some(v) => Ok(*v),
            None => Err(Error::Undefine),
        }
    }
}
