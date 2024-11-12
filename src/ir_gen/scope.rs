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
    symbol_tables: Vec<HashMap<&'ast str, SymbolValue>>,
}

impl<'ast> Scope<'ast> {
    pub(crate) fn new(
        function: Option<Function>,
        instructions: Vec<Value>,
        symbol_tables: Vec<HashMap<&'ast str, SymbolValue>>,
    ) -> Self {
        Self {
            function,
            instructions,
            symbol_tables,
        }
    }

    pub fn is_curr_scope_exist(&self, k: &'ast str) -> bool {
        self.symbol_tables.last().unwrap().contains_key(k)
    }

    pub fn add(&mut self, k: &'ast str, v: SymbolValue) -> Result<()> {
        let table = self.symbol_tables.last_mut().unwrap();
        if let Some(already_exist) = table.insert(k, v) {
            match already_exist {
                SymbolValue::Variable(_) => Ok(()),
                SymbolValue::Const(_) => Err(Error::ReassignConst(k.to_string())),
            }
        } else {
            Ok(())
        }
    }

    pub fn get(&self, k: &'ast str) -> Result<SymbolValue> {
        for symbol in self.symbol_tables.iter().rev() {
            if let Some(v) = symbol.get(k) {
                return Ok(*v);
            };
        }
        Err(Error::Undefine)
    }

    pub fn enter(&mut self) {
        self.symbol_tables.push(HashMap::new());
    }

    pub fn exit(&mut self) {
        self.symbol_tables.pop();
    }
}
