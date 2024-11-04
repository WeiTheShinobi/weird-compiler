use koopa::ir::builder_traits::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::{BinaryOp, FunctionData, Program, Type, Value};
use std::mem;

use crate::ast::*;
use crate::ir_gen::scope::Scope;

use super::eval::Evaluate;

macro_rules! curr_func_mut {
    ($program:expr, $scope:expr) => {
        $program.func_mut($scope.function.unwrap())
    };
}

macro_rules! new_value {
    ($program:expr, $scope:expr) => {
        $program
            .func_mut($scope.function.unwrap())
            .dfg_mut()
            .new_value()
    };
}

pub trait Generate {
    fn generate<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<Value>;
}

impl Generate for CompUnit {
    fn generate<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<Value> {
        self.func_def.generate(program, scope)
    }
}

impl Generate for FuncDef {
    fn generate<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<Value> {
        let func = program.new_func(FunctionData::new_decl(
            format!("@{}", self.ident),
            Vec::new(),
            Type::get_i32(),
        ));
        scope.function = Some(func);

        let entry = curr_func_mut!(program, scope)
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%entry".into()));
        curr_func_mut!(program, scope)
            .layout_mut()
            .bbs_mut()
            .extend([entry]);
        for item in &self.block.block_item {
            item.generate(program, scope);
        }
        curr_func_mut!(program, scope)
            .layout_mut()
            .bb_mut(entry)
            .insts_mut()
            .extend(mem::take(&mut scope.instructions));
        None
    }
}

impl Generate for BlockItem {
    fn generate<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<Value> {
        match self {
            BlockItem::Decl(decl) => decl.generate(program, scope),
            BlockItem::Stmt(stmt) => stmt.generate(program, scope),
        }
    }
}

impl Generate for Decl {
    fn generate<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<Value> {
        match self {
            Decl::ConstDecl(const_decl) => const_decl.generate(program, scope),
            Decl::VarDecl(var_decl) => todo!(),
        }
    }
}

impl Generate for ConstDecl {
    fn generate<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<Value> {
        match self.btype {
            BType::Int => {
                for const_def in &self.defs {
                    const_def.generate(program, scope);
                }
                None
            }
        }
    }
}

impl Generate for ConstDef {
    fn generate<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<Value> {
        let r_val = self.const_init_val.generate(program, scope);
        scope.symbol_table.insert(&self.ident, r_val?);
        r_val
    }
}

impl Generate for ConstInitVal {
    fn generate<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<Value> {
        match self {
            ConstInitVal::ConstExp(const_exp) => const_exp.generate(program, scope),
        }
    }
}

impl Generate for ConstExp {
    fn generate<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<Value> {
        match self {
            ConstExp::Exp(exp) => {
                let n = exp.eval(program, scope).unwrap();
                Some(
                    curr_func_mut!(program, scope)
                        .dfg_mut()
                        .new_value()
                        .integer(n),
                )
            }
        }
    }
}

impl Generate for Stmt {
    fn generate<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<Value> {
        match self {
            Stmt::Return(exp) => {
                let return_val = exp.generate(program, scope);
                let ret = curr_func_mut!(program, scope)
                    .dfg_mut()
                    .new_value()
                    .ret(return_val);
                scope.instructions.push(ret);
                Some(ret)
            }
            Stmt::Assign(lval, exp) => todo!(),
        }
    }
}

impl Generate for Exp {
    fn generate<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<Value> {
        match self {
            Exp::LOrExp(exp) => exp.generate(program, scope),
        }
    }
}

impl Generate for LOrExp {
    fn generate<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<Value> {
        match self {
            LOrExp::LAndExp(land_exp) => land_exp.generate(program, scope),
            LOrExp::LOrExp(lor_exp, land_exp) => {
                let lhs = lor_exp.generate(program, scope)?;
                let rhs = land_exp.generate(program, scope)?;
                let zero = new_value!(program, scope).integer(0);
                let one = new_value!(program, scope).integer(1);

                let inst1 = new_value!(program, scope).binary(BinaryOp::NotEq, lhs, zero);
                let inst2 = new_value!(program, scope).binary(BinaryOp::NotEq, rhs, zero);
                let inst3 = new_value!(program, scope).binary(BinaryOp::Add, inst1, inst2);
                let inst4 = new_value!(program, scope).binary(BinaryOp::Ge, inst3, one);

                scope.instructions.extend([inst1, inst2, inst3, inst4]);
                Some(inst4)
            }
        }
    }
}

impl Generate for LAndExp {
    fn generate<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<Value> {
        match self {
            LAndExp::EqExp(eq_exp) => eq_exp.generate(program, scope),
            LAndExp::LAndExp(land_exp, eq_exp) => {
                let lhs = land_exp.generate(program, scope)?;
                let rhs: Value = eq_exp.generate(program, scope)?;
                let zero = new_value!(program, scope).integer(0);
                let two = new_value!(program, scope).integer(2);

                let inst1 = new_value!(program, scope).binary(BinaryOp::NotEq, lhs, zero);
                let inst2 = new_value!(program, scope).binary(BinaryOp::NotEq, rhs, zero);
                let inst3 = new_value!(program, scope).binary(BinaryOp::Add, inst1, inst2);
                let inst4 = new_value!(program, scope).binary(BinaryOp::Eq, inst3, two);

                scope.instructions.extend([inst1, inst2, inst3, inst4]);
                Some(inst4)
            }
        }
    }
}

impl Generate for EqExp {
    fn generate<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<Value> {
        match self {
            EqExp::RelExp(rel_exp) => rel_exp.generate(program, scope),
            EqExp::EqExp(eq_exp, eq_op, rel_exp) => {
                let lhs = eq_exp.generate(program, scope)?;
                let rhs = rel_exp.generate(program, scope)?;
                let op = match eq_op {
                    EqOp::Eq => BinaryOp::Eq,
                    EqOp::NotEq => BinaryOp::NotEq,
                };
                let inst = curr_func_mut!(program, scope)
                    .dfg_mut()
                    .new_value()
                    .binary(op, lhs, rhs);
                scope.instructions.push(inst);
                Some(inst)
            }
        }
    }
}

impl Generate for RelExp {
    fn generate<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<Value> {
        match self {
            RelExp::AddExp(add_exp) => add_exp.generate(program, scope),
            RelExp::RelExp(rel_exp, rel_op, add_exp) => {
                let lhs = rel_exp.generate(program, scope)?;
                let rhs = add_exp.generate(program, scope)?;
                let op = match rel_op {
                    RelOp::Gt => BinaryOp::Gt,
                    RelOp::Lt => BinaryOp::Lt,
                    RelOp::Ge => BinaryOp::Ge,
                    RelOp::Le => BinaryOp::Le,
                };
                let inst = curr_func_mut!(program, scope)
                    .dfg_mut()
                    .new_value()
                    .binary(op, lhs, rhs);
                scope.instructions.push(inst);
                Some(inst)
            }
        }
    }
}

impl Generate for AddExp {
    fn generate<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<Value> {
        match self {
            AddExp::MulExp(mul_exp) => mul_exp.generate(program, scope),
            AddExp::AddAndMul(add_exp, add_op, mul_exp) => {
                let rhs = mul_exp.generate(program, scope)?;
                let lhs = add_exp.generate(program, scope)?;
                let op = match add_op {
                    AddOp::Add => BinaryOp::Add,
                    AddOp::Sub => BinaryOp::Sub,
                };
                let inst = curr_func_mut!(program, scope)
                    .dfg_mut()
                    .new_value()
                    .binary(op, lhs, rhs);
                scope.instructions.push(inst);
                Some(inst)
            }
        }
    }
}

impl Generate for MulExp {
    fn generate<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<Value> {
        match self {
            MulExp::UnaryExp(unary_exp) => unary_exp.generate(program, scope),
            MulExp::MulAndUnary(mul_exp, mul_op, unary_exp) => {
                let lhs = mul_exp.generate(program, scope)?;
                let rhs = unary_exp.generate(program, scope)?;
                let op = match mul_op {
                    MulOp::Mul => BinaryOp::Mul,
                    MulOp::Div => BinaryOp::Div,
                    MulOp::Mod => BinaryOp::Mod,
                };
                let inst = curr_func_mut!(program, scope)
                    .dfg_mut()
                    .new_value()
                    .binary(op, lhs, rhs);
                scope.instructions.push(inst);
                Some(inst)
            }
        }
    }
}

impl Generate for UnaryExp {
    fn generate<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<Value> {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.generate(program, scope),
            UnaryExp::UnaryOp(unary_op, unary_exp) => match unary_op {
                UnaryOp::Add => unary_exp.generate(program, scope),
                UnaryOp::Minus => {
                    let l_value = unary_exp.generate(program, scope)?;
                    let r_value = curr_func_mut!(program, scope)
                        .dfg_mut()
                        .new_value()
                        .integer(0);
                    let inst = curr_func_mut!(program, scope).dfg_mut().new_value().binary(
                        BinaryOp::Sub,
                        r_value,
                        l_value,
                    );
                    scope.instructions.push(inst);
                    Some(inst)
                }
                UnaryOp::Not => {
                    let l_value = unary_exp.generate(program, scope)?;
                    let r_value = curr_func_mut!(program, scope)
                        .dfg_mut()
                        .new_value()
                        .integer(0);
                    let inst = curr_func_mut!(program, scope).dfg_mut().new_value().binary(
                        BinaryOp::Eq,
                        r_value,
                        l_value,
                    );
                    scope.instructions.push(inst);
                    Some(inst)
                }
            },
        }
    }
}

impl Generate for PrimaryExp {
    fn generate<'ast>(&'ast self, program: &mut Program, scope: &mut Scope<'ast>) -> Option<Value> {
        match self {
            PrimaryExp::Expression(exp) => exp.generate(program, scope),
            PrimaryExp::Number(n) => Some(
                curr_func_mut!(program, scope)
                    .dfg_mut()
                    .new_value()
                    .integer(*n),
            ),
            PrimaryExp::LVal(lval) => match scope.symbol_table.get(lval.ident.as_str()) {
                Some(val) => Some(val.clone()),
                None => unimplemented!("use undefine symbol but error handling not yet finished"),
            },
        }
    }
}
