use koopa::ir::{Function, Value};
use std::collections::HashMap;


pub(crate) struct Scope<'ast> {
    pub(crate) function: Option<Function>,
    pub(crate) instructions: Vec<Value>,
    pub(crate) symbol_table: HashMap<&'ast str, Value>,
}

impl<'ast> Scope<'ast> {
    pub(crate) fn new(
        function: Option<Function>,
        instructions: Vec<Value>,
        symbol_table: HashMap<&'ast str, Value>,
    ) -> Self {
        Self {
            function,
            instructions,
            symbol_table,
        }
    }
}
