use super::{Error, Result};
use crate::ir_gen::gen::SymbolValue;
use koopa::ir::{BasicBlock, Function, Value};
use std::collections::HashMap;


pub struct Scope<'ast> {
    pub function: Option<Function>,
    pub instructions: Vec<Value>,
    symbol_tables: Vec<HashMap<&'ast str, SymbolValue>>,
    curr_bb: Option<BasicBlock>,
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
            curr_bb: None,
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

    pub fn curr_bb(&self) -> BasicBlock {
        self.curr_bb.unwrap()
    }
    pub fn push_inst(&mut self, value: Value) {
        self.instructions.push(value);
    }

    pub fn set_bb(&mut self, bb: BasicBlock) {
        self.curr_bb = Some(bb);
    }
}
