use koopa::front::ast::*;
use koopa::ir::builder_traits::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::{FunctionData, Program, Type};

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
        let func = program.new_func(FunctionData::new(
            format!("@{}", self.ident),
            Vec::new(),
            Type::get_i32(),
        ));
        let func_data = program.func_mut(func);

        let entry = func_data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%entry".into()));
        func_data.layout_mut().bbs_mut().extend([entry]);

        let ret_val = func_data.dfg_mut().new_value().integer(self.block.stmt.num);

        let ret = func_data.dfg_mut().new_value().ret(Some(ret_val));
        func_data
            .layout_mut()
            .bb_mut(entry)
            .insts_mut()
            .extend([ret]);
    }
}



impl Compile for Stmt {
    fn compile(&self, func_data: &mut FunctionData) {
        match self {
            Stmt::Return(exp) => {
                exp.compile(func_data);
            }
        }
    }
}

impl Compile for Exp {
    fn compile(&self, func_data: &mut FunctionData) {
        match self {
            Exp::UnaryExp(exp) => {
                exp.compile(func_data);
            }
        }
    }
}

impl Compile for UnaryExp {
    fn compile(&self, func_data: &mut FunctionData) {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.compile(func_data),
            UnaryExp::UnaryOp(unary_op, unary_exp) => {
                match unary_op {
                    UnaryOp::Add => ,
                    UnaryOp::Minus => todo!(),
                    UnaryOp::Not => todo!(),
                }
            },
        }
    }
}

impl Compile for PrimaryExp {
    fn compile(&self, func_data: &mut FunctionData) {
        match self {
            PrimaryExp::Expression(exp) => exp.compile(func_data),
            PrimaryExp::Number(n) => ,
        }
    }
}
