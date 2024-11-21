use super::{Error, Result};
use crate::ir_gen::gen::SymbolValue;
use koopa::ir::{BasicBlock, Function, Value};
use std::collections::HashMap;
use std::thread::sleep;
use crate::ir_gen::Error::NoInLoop;

pub struct Scope<'ast> {
    pub function: Option<Function>,
    pub instructions: Vec<Value>,
    loop_stack: Vec<LoopBlock>,
    symbol_tables: Vec<HashMap<&'ast str, SymbolValue>>,
    curr_bb: Option<BasicBlock>,
}

#[derive(Copy, Clone)]
pub struct LoopBlock {
    pub entry: BasicBlock,
    pub exit: BasicBlock,
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
            loop_stack: vec![],
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
        Err(Error::Undefined)
    }

    /// { // enter scope    
    /// } // exit scope
    pub fn enter_scope(&mut self) {
        self.symbol_tables.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.symbol_tables.pop();
    }

    pub fn curr_bb(&self) -> BasicBlock {
        self.curr_bb.unwrap()
    }

    pub fn set_bb(&mut self, bb: BasicBlock) {
        self.curr_bb = Some(bb);
    }

    pub fn enter_loop(&mut self, entry: BasicBlock, exit: BasicBlock) {
        self.loop_stack.push(LoopBlock { entry, exit })
    }

    pub fn exit_loop(&mut self) {
        self.loop_stack.pop();
    }
    pub fn get_loop_block(&mut self) -> Result<LoopBlock> {
        if let Some(bbs) = self.loop_stack.last() {
            Ok(bbs.clone())
        } else {
            Err(NoInLoop)
        }
    }
}
