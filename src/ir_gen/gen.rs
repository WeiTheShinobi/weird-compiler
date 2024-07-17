use koopa::ir::{FunctionData, Program, Type};
use koopa::ir::builder_traits::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};

use crate::ast::*;

pub trait Generate {
    fn generate(&self, program: &mut Program);
}

impl Generate for CompUnit {
    fn generate(&self, program: &mut Program) {
        self.func_def.generate(program);
    }
}

impl Generate for FuncDef {
    fn generate(&self, program: &mut Program) {
        let func = program.new_func(FunctionData::new(format!("@{}", self.ident), Vec::new(), Type::get_i32()));
        let func_data = program.func_mut(func);

        let entry = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));
        func_data.layout_mut().bbs_mut().extend([entry]);

        let ret_val = func_data.dfg_mut().new_value().integer(self.block.stmt.num);
        let ret = func_data.dfg_mut().new_value().ret(Some(ret_val));
        func_data.layout_mut().bb_mut(entry).insts_mut().extend([ret]);
    }
}
