use crate::riscv_gen::gen::AsmValue;
use koopa::ir::{Function, Type, Value};
use std::collections::HashMap;

pub struct Context {
    pub function_table: HashMap<Function, String>,
    // should clear below field when change function
    pub stack_size: usize,
    pub stack_used_size: usize,
    pub ra_pos: Option<usize>,
    pub symbol_table: HashMap<Value, AsmValue>,
}

impl Context {
    pub fn new() -> Context {
        Context {
            function_table: HashMap::new(),
            stack_size: 0,
            stack_used_size: 0,
            ra_pos: None,
            symbol_table: HashMap::new(),
        }
    }

    pub fn clear(&mut self) {
        self.stack_size = 0;
        self.stack_used_size = 0;
        self.symbol_table.clear();
    }

    pub fn get_stack_space(&mut self, size: usize) -> String {
        let start_pos = self.stack_used_size;
        self.stack_used_size += size;

        assert!(
            self.stack_used_size <= self.stack_size - 4,
            "used size: {}, stack size: {}",
            self.stack_used_size,
            self.stack_size
        );
        format!("{}(sp)", start_pos)
    }

    pub fn get_symbol(&self, key: &Value) -> Option<&AsmValue> {
        self.symbol_table.get(key)
    }

    pub fn set_symbol(&mut self, k: Value, v: AsmValue) {
        self.symbol_table.insert(k, v);
    }
}
