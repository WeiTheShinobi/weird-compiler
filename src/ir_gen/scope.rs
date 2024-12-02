use super::{Error, Result};
use crate::ir_gen::gen::SymbolValue;
use crate::ir_gen::Error::NoInLoop;
use koopa::ir::{BasicBlock, Function};
use std::collections::HashMap;

pub struct Scope<'ast> {
    pub function: Option<Function>,
    pub global: Global<'ast>,
    loop_stack: Vec<LoopBlock>,
    symbol_tables: Vec<HashMap<&'ast str, SymbolValue>>,
    curr_bb: Option<BasicBlock>,
}

#[derive(Clone)]
pub struct Global<'ast> {
    pub function: HashMap<&'ast str, Function>,
    pub decl: HashMap<&'ast str, SymbolValue>,
}

impl<'ast> Global<'ast> {
    pub fn new() -> Self {
        Global {
            function: HashMap::new(),
            decl: HashMap::new(),
        }
    }
}

#[derive(Copy, Clone)]
pub struct LoopBlock {
    pub entry: BasicBlock,
    pub exit: BasicBlock,
}

impl<'ast> Scope<'ast> {
    pub(crate) fn new(
        function: Option<Function>,
        global: Global<'ast>,
        symbol_tables: Vec<HashMap<&'ast str, SymbolValue>>,
    ) -> Self {
        Self {
            function,
            global,
            loop_stack: vec![],
            symbol_tables,
            curr_bb: None,
        }
    }

    pub fn in_global_scope(&self) -> bool {
        self.function.is_none()
    }

    pub fn reset_symbol_table(&mut self) {
        self.symbol_tables.clear();
        self.function = None;
    }

    pub fn is_curr_scope_exist(&self, k: &'ast str) -> bool {
        match self.symbol_tables.last() {
            Some(map) => {
                map.contains_key(k)
            }
            None => self.global.decl.contains_key(k)
        }
    }

    pub fn add(&mut self, k: &'ast str, v: SymbolValue) -> Result<()> {
        let table = self.symbol_tables.last_mut().unwrap();
        if let Some(_) = self.global.decl.get(k) {
            return Err(Error::Redeclare(format!("name: {}", k)));
        }
        if let Some(already_exist) = table.insert(k, v) {
            match already_exist {
                SymbolValue::NeedLoad(_) => Ok(()),
                SymbolValue::Value(_) | SymbolValue::GlobalConst(_) => Err(Error::ReassignConst(k.to_string())),
            }
        } else {
            Ok(())
        }
    }

    pub fn add_global_decl(&mut self, k: &'ast str, v: SymbolValue) -> Result<()> {
        for table in self.symbol_tables.iter() {
            if let Some(_) = table.get(k) {
                return Err(Error::Redeclare(format!("name: {}", k)));
            }
        }
        if let Some(already_exist) = self.global.decl.insert(k, v) {
            match already_exist {
                SymbolValue::NeedLoad(_) => Ok(()),
                SymbolValue::Value(_) | SymbolValue::GlobalConst(_) => Err(Error::ReassignConst(k.to_string())),
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
        if let Some(v) = self.global.decl.get(k) {
            return Ok(*v);
        };
        Err(Error::Undefined(format!("not found : {:?}", k)))
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
