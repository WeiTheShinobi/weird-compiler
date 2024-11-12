use koopa::ir::builder_traits::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::{BinaryOp, FunctionData, Program, Type};
use std::mem;

use crate::ast::*;
use crate::ir_gen::scope::Scope;

use super::eval::Evaluate;
use super::scope::SymbolValue;
use super::{Error, Result};

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

macro_rules! value_dbg {
    ($program:expr, $scope:expr, $value:expr) => {
        dbg!($program.func($scope.function.unwrap()).dfg().value($value));
    };
}

pub trait Generate {
    type Out;

    fn generate<'ast>(
        &'ast self,
        program: &mut Program,
        scope: &mut Scope<'ast>,
    ) -> Result<Self::Out>;
}

impl Generate for CompUnit {
    type Out = ();

    fn generate<'ast>(
        &'ast self,
        program: &mut Program,
        scope: &mut Scope<'ast>,
    ) -> Result<Self::Out> {
        self.func_def.generate(program, scope)?;
        dbg!(self);
        Ok(())
    }
}

impl Generate for FuncDef {
    type Out = ();

    fn generate<'ast>(
        &'ast self,
        program: &mut Program,
        scope: &mut Scope<'ast>,
    ) -> Result<Self::Out> {
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

        self.block.generate(program, scope)?;

        curr_func_mut!(program, scope)
            .layout_mut()
            .bb_mut(entry)
            .insts_mut()
            .extend(mem::take(&mut scope.instructions));
        Ok(())
    }
}

impl Generate for Block {
    type Out = ();

    fn generate<'ast>(
        &'ast self,
        program: &mut Program,
        scope: &mut Scope<'ast>,
    ) -> Result<Self::Out> {
        scope.enter();
        for item in &self.block_item {
            item.generate(program, scope)?;
        }
        scope.exit();
        Ok(())
    }
}

impl Generate for BlockItem {
    type Out = ();

    fn generate<'ast>(
        &'ast self,
        program: &mut Program,
        scope: &mut Scope<'ast>,
    ) -> Result<Self::Out> {
        match self {
            BlockItem::Decl(decl) => decl.generate(program, scope),
            BlockItem::Stmt(stmt) => stmt.generate(program, scope),
        }
    }
}

impl Generate for Decl {
    type Out = ();

    fn generate<'ast>(
        &'ast self,
        program: &mut Program,
        scope: &mut Scope<'ast>,
    ) -> Result<Self::Out> {
        match self {
            Decl::ConstDecl(const_decl) => const_decl.generate(program, scope),
            Decl::VarDecl(var_decl) => var_decl.generate(program, scope),
        }
    }
}

impl Generate for VarDecl {
    type Out = ();

    fn generate<'ast>(
        &'ast self,
        program: &mut Program,
        scope: &mut Scope<'ast>,
    ) -> Result<Self::Out> {
        let return_type = match self.btype {
            BType::Int => Type::get_i32(),
        };
        for def in &self.defs {
            let return_type = return_type.clone();
            match def {
                VarDef::Id(id) => {
                    if scope.is_curr_scope_exist(&id) {
                        return Err(Error::Redecalre(id.to_string()));
                    };

                    let var = new_value!(program, scope).alloc(return_type);
                    let zero_value = new_value!(program, scope).zero_init(Type::get_i32());
                    curr_func_mut!(program, scope)
                        .dfg_mut()
                        .set_value_name(zero_value, Some(format!("@{}", id)));
                    scope.add(&id, SymbolValue::Variable(zero_value))?;
                    scope.instructions.extend([var, zero_value]);
                }
                VarDef::Assign(id, init_val) => {
                    if scope.is_curr_scope_exist(&id) {
                        return Err(Error::Redecalre(id.to_string()));
                    };
                    let alloc = new_value!(program, scope).alloc(return_type);
                    curr_func_mut!(program, scope)
                        .dfg_mut()
                        .set_value_name(alloc, Some(format!("@{}", id)));

                    let value = init_val
                        .exp
                        .generate(program, scope)?
                        .into_value(program, scope);
                    let store_value = new_value!(program, scope).store(value, alloc);
                    scope.add(&id, SymbolValue::Variable(alloc))?;
                    scope.instructions.extend([alloc, store_value]);
                }
            }
        }
        Ok(())
    }
}

impl Generate for ConstDecl {
    type Out = ();

    fn generate<'ast>(
        &'ast self,
        program: &mut Program,
        scope: &mut Scope<'ast>,
    ) -> Result<Self::Out> {
        match self.btype {
            BType::Int => {
                for const_def in &self.defs {
                    const_def.generate(program, scope)?
                }
                Ok(())
            }
        }
    }
}

impl Generate for ConstDef {
    type Out = ();

    fn generate<'ast>(
        &'ast self,
        program: &mut Program,
        scope: &mut Scope<'ast>,
    ) -> Result<Self::Out> {
        let r_val = self.const_init_val.generate(program, scope)?;
        scope.add(
            &self.ident,
            SymbolValue::Const(new_value!(program, scope).integer(r_val)),
        )
    }
}

impl Generate for ConstInitVal {
    type Out = i32;

    fn generate<'ast>(
        &'ast self,
        program: &mut Program,
        scope: &mut Scope<'ast>,
    ) -> Result<Self::Out> {
        match self {
            ConstInitVal::ConstExp(const_exp) => const_exp.generate(program, scope),
        }
    }
}

impl Generate for ConstExp {
    type Out = i32;

    fn generate<'ast>(
        &'ast self,
        program: &mut Program,
        scope: &mut Scope<'ast>,
    ) -> Result<Self::Out> {
        match self {
            ConstExp::Exp(exp) => Ok(exp.eval(program, scope).unwrap()),
        }
    }
}

impl Generate for Stmt {
    type Out = ();

    fn generate<'ast>(
        &'ast self,
        program: &mut Program,
        scope: &mut Scope<'ast>,
    ) -> Result<Self::Out> {
        match self {
            Stmt::Return(exp) => {
                let return_val = if let Some(exp) = exp {
                    Some(exp.generate(program, scope)?.into_value(program, scope))
                } else {
                    None
                };
                let ret = new_value!(program, scope).ret(return_val);
                scope.instructions.push(ret);
                Ok(())
            }
            Stmt::Exp(exp) => {
                if let Some(exp) = exp {
                    match exp.generate(program, scope) {
                        Ok(_) => Ok(()),
                        Err(err) => Err(err),
                    }
                } else {
                    Ok(())
                }
            }
            Stmt::Block(block) => block.generate(program, scope),
            Stmt::Assign(lval, exp) => {
                let old_value = scope.get(&lval.ident)?;
                match old_value {
                    SymbolValue::Variable(old) => {
                        let new_val = exp.generate(program, scope)?.into_value(program, scope);
                        let store = new_value!(program, scope).store(new_val, old);
                        scope.instructions.push(store);
                        Ok(())
                    }
                    SymbolValue::Const(_) => Err(Error::ReassignConst(lval.ident.clone())),
                }
            }
        }
    }
}

impl Generate for Exp {
    type Out = SymbolValue;

    fn generate<'ast>(
        &'ast self,
        program: &mut Program,
        scope: &mut Scope<'ast>,
    ) -> Result<Self::Out> {
        match self {
            Exp::LOrExp(exp) => exp.generate(program, scope),
        }
    }
}

impl Generate for LOrExp {
    type Out = SymbolValue;

    fn generate<'ast>(
        &'ast self,
        program: &mut Program,
        scope: &mut Scope<'ast>,
    ) -> Result<Self::Out> {
        match self {
            LOrExp::LAndExp(land_exp) => land_exp.generate(program, scope),
            LOrExp::LOrExp(lor_exp, land_exp) => {
                let lhs = lor_exp.generate(program, scope)?.into_value(program, scope);
                let rhs = land_exp
                    .generate(program, scope)?
                    .into_value(program, scope);
                let zero = new_value!(program, scope).integer(0);
                let one = new_value!(program, scope).integer(1);

                let inst1 = new_value!(program, scope).binary(BinaryOp::NotEq, lhs, zero);
                let inst2 = new_value!(program, scope).binary(BinaryOp::NotEq, rhs, zero);
                let inst3 = new_value!(program, scope).binary(BinaryOp::Add, inst1, inst2);
                let inst4 = new_value!(program, scope).binary(BinaryOp::Ge, inst3, one);

                scope.instructions.extend([inst1, inst2, inst3, inst4]);
                Ok(SymbolValue::Const(inst4))
            }
        }
    }
}

impl Generate for LAndExp {
    type Out = SymbolValue;

    fn generate<'ast>(
        &'ast self,
        program: &mut Program,
        scope: &mut Scope<'ast>,
    ) -> Result<Self::Out> {
        match self {
            LAndExp::EqExp(eq_exp) => eq_exp.generate(program, scope),
            LAndExp::LAndExp(land_exp, eq_exp) => {
                let lhs = land_exp
                    .generate(program, scope)?
                    .into_value(program, scope);
                let rhs = eq_exp.generate(program, scope)?.into_value(program, scope);
                let zero = new_value!(program, scope).integer(0);
                let two = new_value!(program, scope).integer(2);

                let inst1 = new_value!(program, scope).binary(BinaryOp::NotEq, lhs, zero);
                let inst2 = new_value!(program, scope).binary(BinaryOp::NotEq, rhs, zero);
                let inst3 = new_value!(program, scope).binary(BinaryOp::Add, inst1, inst2);
                let inst4 = new_value!(program, scope).binary(BinaryOp::Eq, inst3, two);

                scope.instructions.extend([inst1, inst2, inst3, inst4]);
                Ok(SymbolValue::Const(inst4))
            }
        }
    }
}

impl Generate for EqExp {
    type Out = SymbolValue;

    fn generate<'ast>(
        &'ast self,
        program: &mut Program,
        scope: &mut Scope<'ast>,
    ) -> Result<Self::Out> {
        match self {
            EqExp::RelExp(rel_exp) => rel_exp.generate(program, scope),
            EqExp::EqExp(eq_exp, eq_op, rel_exp) => {
                let lhs = eq_exp.generate(program, scope)?.into_value(program, scope);
                let rhs = rel_exp.generate(program, scope)?.into_value(program, scope);
                let op = match eq_op {
                    EqOp::Eq => BinaryOp::Eq,
                    EqOp::NotEq => BinaryOp::NotEq,
                };
                let inst = curr_func_mut!(program, scope)
                    .dfg_mut()
                    .new_value()
                    .binary(op, lhs, rhs);
                scope.instructions.push(inst);
                Ok(SymbolValue::Const(inst))
            }
        }
    }
}

impl Generate for RelExp {
    type Out = SymbolValue;

    fn generate<'ast>(
        &'ast self,
        program: &mut Program,
        scope: &mut Scope<'ast>,
    ) -> Result<Self::Out> {
        match self {
            RelExp::AddExp(add_exp) => add_exp.generate(program, scope),
            RelExp::RelExp(rel_exp, rel_op, add_exp) => {
                let lhs = rel_exp.generate(program, scope)?.into_value(program, scope);
                let rhs = add_exp.generate(program, scope)?.into_value(program, scope);
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
                Ok(SymbolValue::Const(inst))
            }
        }
    }
}

impl Generate for AddExp {
    type Out = SymbolValue;

    fn generate<'ast>(
        &'ast self,
        program: &mut Program,
        scope: &mut Scope<'ast>,
    ) -> Result<Self::Out> {
        match self {
            AddExp::MulExp(mul_exp) => mul_exp.generate(program, scope),
            AddExp::AddAndMul(add_exp, add_op, mul_exp) => {
                let lhs = add_exp.generate(program, scope)?.into_value(program, scope);
                let rhs = mul_exp.generate(program, scope)?.into_value(program, scope);
                let op = match add_op {
                    AddOp::Add => BinaryOp::Add,
                    AddOp::Sub => BinaryOp::Sub,
                };
                let inst = curr_func_mut!(program, scope)
                    .dfg_mut()
                    .new_value()
                    .binary(op, lhs, rhs);
                scope.instructions.push(inst);
                Ok(SymbolValue::Const(inst))
            }
        }
    }
}

impl Generate for MulExp {
    type Out = SymbolValue;

    fn generate<'ast>(
        &'ast self,
        program: &mut Program,
        scope: &mut Scope<'ast>,
    ) -> Result<Self::Out> {
        match self {
            MulExp::UnaryExp(unary_exp) => unary_exp.generate(program, scope),
            MulExp::MulAndUnary(mul_exp, mul_op, unary_exp) => {
                let lhs = mul_exp.generate(program, scope)?.into_value(program, scope);
                let rhs = unary_exp
                    .generate(program, scope)?
                    .into_value(program, scope);
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
                Ok(SymbolValue::Const(inst))
            }
        }
    }
}

impl Generate for UnaryExp {
    type Out = SymbolValue;

    fn generate<'ast>(
        &'ast self,
        program: &mut Program,
        scope: &mut Scope<'ast>,
    ) -> Result<Self::Out> {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.generate(program, scope),
            UnaryExp::UnaryOp(unary_op, unary_exp) => match unary_op {
                UnaryOp::Add => unary_exp.generate(program, scope),
                UnaryOp::Minus => {
                    let l_value = unary_exp
                        .generate(program, scope)?
                        .into_value(program, scope);
                    let r_value = new_value!(program, scope).integer(0);
                    let inst = new_value!(program, scope).binary(BinaryOp::Sub, r_value, l_value);
                    scope.instructions.push(inst);
                    Ok(SymbolValue::Const(inst))
                }
                UnaryOp::Not => {
                    let l_value = unary_exp
                        .generate(program, scope)?
                        .into_value(program, scope);
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
                    Ok(SymbolValue::Const(inst))
                }
            },
        }
    }
}

impl Generate for PrimaryExp {
    type Out = SymbolValue;

    fn generate<'ast>(
        &'ast self,
        program: &mut Program,
        scope: &mut Scope<'ast>,
    ) -> Result<Self::Out> {
        match self {
            PrimaryExp::Expression(exp) => exp.generate(program, scope),
            PrimaryExp::Number(n) => Ok(SymbolValue::Const(new_value!(program, scope).integer(*n))),
            PrimaryExp::LVal(lval) => match scope.get(&lval.ident) {
                Ok(v) => Ok(v),
                Err(err) => Err(err),
            },
        }
    }
}
