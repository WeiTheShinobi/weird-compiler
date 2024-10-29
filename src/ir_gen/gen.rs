use koopa::ir::builder_traits::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::{BinaryOp, FunctionData, Program, Type, Value};

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
        let insts = Vec::<Value>::new();
        let mut scope = Scope::new(func_data, insts);

        let entry = scope.function_data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%entry".into()));
        scope.function_data.layout_mut().bbs_mut().extend([entry]);
        self.block.stmt.eval(&mut scope);

        scope.function_data
            .layout_mut()
            .bb_mut(entry)
            .insts_mut()
            .extend(scope.instructions);
    }
}

struct Scope<'a> {
    function_data: &'a mut FunctionData,
    instructions: Vec<Value>,
}


impl<'a> Scope<'a> {
    fn new(function_data: &'a mut FunctionData, instructions: Vec<Value>) -> Self {
        Self {
            function_data,
            instructions,
        }
    }

    fn instructions_debug(&self) {
        println!("---");
        for &ins in &self.instructions {
            println!("{:?}", self.function_data.dfg().value(ins));
        }
        println!("---");
    }
}

trait Evaluate {
    fn eval(&self, scope: &mut Scope) -> Option<Value>;
}

impl Evaluate for Stmt {
    fn eval(&self, scope: &mut Scope) -> Option<Value> {
        match self {
            Stmt::Return(exp) => {
                let return_val = exp.eval(scope);
                let ret = scope.function_data.dfg_mut().new_value().ret(return_val);
                scope.instructions.push(ret);
                Some(ret)
            }
        }
    }
}

impl Evaluate for Exp {
    fn eval(&self, scope: &mut Scope) -> Option<Value> {
        match self {
            Exp::UnaryExp(exp) => exp.eval(scope)
        }
    }
}

impl Evaluate for UnaryExp {
    fn eval(&self, scope: &mut Scope) -> Option<Value> {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.eval(scope),
            UnaryExp::UnaryOp(unary_op, unary_exp) => {
                match unary_op {
                    UnaryOp::Add => unary_exp.eval(scope),
                    UnaryOp::Minus => {
                        let l_value = unary_exp.eval(scope).unwrap();
                        let r_value = scope.function_data.dfg_mut().new_value().integer(0);
                        let inst =
                            scope.function_data
                                .dfg_mut()
                                .new_value()
                                .binary(BinaryOp::Sub, r_value, l_value);
                        scope.instructions.push(inst);
                        Some(inst)
                    }
                    UnaryOp::Not => {
                        let l_value = unary_exp.eval(scope).unwrap();
                        let r_value = scope.function_data.dfg_mut().new_value().integer(0);
                        let inst =
                            scope.function_data
                                .dfg_mut()
                                .new_value()
                                .binary(BinaryOp::Eq, r_value, l_value);
                        scope.instructions.push(inst);
                        Some(inst)
                    }
                }
            }
        }
    }
}

impl Evaluate for PrimaryExp {
    fn eval(&self, scope: &mut Scope) -> Option<Value> {
        match self {
            PrimaryExp::Expression(exp) => exp.eval(scope),
            PrimaryExp::Number(n) => Some(scope.function_data.dfg_mut().new_value().integer(*n)),
        }
    }
}
