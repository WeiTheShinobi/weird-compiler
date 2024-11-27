use std::fmt::format;
use std::panic::panic_any;

use koopa::ir::builder_traits::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Program, Type, TypeKind, Value, ValueKind};

use crate::ast::*;
use crate::ir_gen::scope::Scope;
use crate::ir_gen::Error::Undefined;

use super::eval::Evaluate;
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

macro_rules! new_bb {
    ($program:expr, $scope:expr) => {
        $program
            .func_mut($scope.function.unwrap())
            .dfg_mut()
            .new_bb()
    };
}

macro_rules! add_bb_to_program {
    ($program:expr, $scope:expr, $bb: expr) => {
        curr_func_mut!($program, $scope)
            .layout_mut()
            .bbs_mut()
            .extend([$bb]);
    };
}

macro_rules! push_insts {
    ($program:expr, $scope:expr, $($inst:expr),*) => {
        curr_func_mut!($program, $scope)
            .layout_mut()
            .bb_mut($scope.curr_bb())
            .insts_mut()
            .extend([$($inst),*]);
    };
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolValue {
    NeedLoad(Value),
    Value(Value),
}

impl SymbolValue {
    pub fn into_value(self, program: &mut Program, scope: &mut Scope) -> Value {
        match self {
            SymbolValue::NeedLoad(value) => {
                dbg!(curr_func_mut!(program, scope).dfg().value(value));
                let v = new_value!(program, scope).load(value);
                push_insts!(program, scope, v);
                v
            }
            SymbolValue::Value(value) => value,
        }
    }
}

/// if last inst isn't "Return", add jump inst and return true
fn maybe_add_jump(program: &mut Program, scope: &mut Scope, jump_to: BasicBlock) {
    if let Some(last_inst) = curr_func_mut!(program, scope)
        .layout_mut()
        .bb_mut(scope.curr_bb())
        .insts()
        .back_key()
    {
        let last_inst = last_inst.clone();
        let last_inst_data = curr_func_mut!(program, scope).dfg().value(last_inst);
        if let ValueKind::Return(_) = last_inst_data.kind() {} else {
            let jmp = new_value!(program, scope).jump(jump_to);
            push_insts!(program, scope, jmp);
        }
    } else {
        let jmp = new_value!(program, scope).jump(jump_to);
        push_insts!(program, scope, jmp);
    }
}

fn maybe_add_return(program: &mut Program, scope: &mut Scope) {
    let last_inst = match curr_func_mut!(program, scope)
        .layout_mut()
        .bb_mut(scope.curr_bb())
        .insts()
        .back_key() {
        Some(last_inst) => last_inst.clone(),
        None => {
            let ret = new_value!(program, scope).ret(None);
            push_insts!(program, scope, ret);
            return;
        }
    };

    let last_inst = last_inst.clone();
    let last_inst_data = curr_func_mut!(program, scope).dfg().value(last_inst);
    if let ValueKind::Return(_) = last_inst_data.kind() {} else {
        let ret = new_value!(program, scope).ret(None);
        push_insts!(program, scope, ret);
    }
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
        dbg!(&self.func_def);
        if let Some(ref comp_unit) = *self.comp_unit {
            comp_unit.generate(program, scope)?
        }
        self.func_def.generate(program, scope)?;
        scope.reset_symbol_table();
        Ok(())
    }
}
fn btype_to_ir_type(ty: BType) -> Type {
    match ty {
        BType::Int => Type::get_i32(),
    }
}

fn param_to_ir_type(param: &FuncFParam) -> (Option<String>, Type) {
    match param.btype {
        BType::Int => (Some(format!("@{}", param.ident.clone())), Type::get_i32()),
    }
}

impl Generate for FuncDef {
    type Out = ();

    fn generate<'ast>(
        &'ast self,
        program: &mut Program,
        scope: &mut Scope<'ast>,
    ) -> Result<Self::Out> {
        let params_ty = self
            .params
            .iter()
            .map(param_to_ir_type)
            .collect();
        let return_ty = match self.func_type {
            FuncType::Int => Type::get_i32(),
            FuncType::Void => Type::get_unit(),
        };
        let func = program.new_func(FunctionData::with_param_names(
            format!("@{}", self.ident),
            params_ty,
            return_ty,
        ));

        scope.function = Some(func);
        scope.global.function.insert(self.ident.as_str(), func);

        let entry = new_bb!(program, scope).basic_block(Some("%entry".into()));
        add_bb_to_program!(program, scope, entry);
        scope.set_bb(entry);
        scope.enter_scope();

        for i in 0..self.params.len() {
            let param = program.func(scope.function.unwrap()).params()[i].clone();
            let data = curr_func_mut!(program,scope).dfg_mut().value(param);
            let ty = data.ty().clone();
            let name = data.name().clone().unwrap();
            let name = name.trim_start_matches("@");
            let p_var = new_value!(program, scope).alloc(ty);
            curr_func_mut!(program, scope)
                .dfg_mut()
                .set_value_name(p_var, Some(format!("%{}", name)));
            let store = new_value!(program, scope).store(param, p_var);
            let load = new_value!(program, scope).load(p_var);
            push_insts!(program, scope, p_var, store, load);
            scope.add(self.params[i].ident.as_str(), SymbolValue::Value(load))?;
        }

        self.block.generate(program, scope)?;
        maybe_add_return(program, scope);
        scope.exit_scope();
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
        scope.enter_scope();
        for item in &self.block_item {
            item.generate(program, scope)?;
        }
        scope.exit_scope();
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
                        return Err(Error::Redeclare(id.to_string()));
                    };

                    let var = new_value!(program, scope).alloc(return_type);
                    let zero_value = new_value!(program, scope).zero_init(Type::get_i32());
                    curr_func_mut!(program, scope)
                        .dfg_mut()
                        .set_value_name(zero_value, Some(format!("@{}", id)));
                    scope.add(&id, SymbolValue::NeedLoad(zero_value))?;
                    push_insts!(program, scope, var, zero_value);
                }
                VarDef::Assign(id, init_val) => {
                    if scope.is_curr_scope_exist(&id) {
                        return Err(Error::Redeclare(id.to_string()));
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
                    scope.add(&id, SymbolValue::NeedLoad(alloc))?;
                    push_insts!(program, scope, alloc, store_value);
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
            SymbolValue::Value(new_value!(program, scope).integer(r_val)),
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
                push_insts!(program, scope, ret);
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
                    SymbolValue::NeedLoad(old) => {
                        let new_val = exp.generate(program, scope)?.into_value(program, scope);
                        let store = new_value!(program, scope).store(new_val, old);

                        push_insts!(program, scope, store);
                        Ok(())
                    }
                    SymbolValue::Value(_) => Err(Error::ReassignConst(lval.ident.clone())),
                }
            }
            Stmt::If(if_stmt) => {
                let cond = if_stmt
                    .cond
                    .generate(program, scope)?
                    .into_value(program, scope);

                if let Some(else_stmt) = &if_stmt.else_then {
                    let if_block = new_bb!(program, scope).basic_block(Some("%then".to_string()));
                    add_bb_to_program!(program, scope, if_block);
                    let else_block = new_bb!(program, scope).basic_block(Some("%else".to_string()));
                    add_bb_to_program!(program, scope, else_block);
                    let br_end = new_bb!(program, scope).basic_block(Some("%br_end".to_string()));
                    add_bb_to_program!(program, scope, br_end);

                    let br = new_value!(program, scope).branch(cond, if_block, else_block);
                    push_insts!(program, scope, br);

                    scope.set_bb(if_block);
                    if_stmt.if_then.generate(program, scope)?;
                    maybe_add_jump(program, scope, br_end);

                    scope.set_bb(else_block);
                    else_stmt.generate(program, scope)?;
                    maybe_add_jump(program, scope, br_end);

                    scope.set_bb(br_end);
                } else {
                    let if_block = new_bb!(program, scope).basic_block(Some("%then".to_string()));
                    add_bb_to_program!(program, scope, if_block);

                    let br_end = new_bb!(program, scope).basic_block(Some("%br_end".to_string()));
                    add_bb_to_program!(program, scope, br_end);

                    let br = new_value!(program, scope).branch(cond, if_block, br_end);
                    push_insts!(program, scope, br);

                    scope.set_bb(if_block);
                    if_stmt.if_then.generate(program, scope)?;
                    maybe_add_jump(program, scope, br_end);

                    scope.set_bb(br_end);
                }
                Ok(())
            }
            Stmt::While(while_stmt) => {
                let while_cond =
                    new_bb!(program, scope).basic_block(Some("%while_cond".to_string()));
                add_bb_to_program!(program, scope, while_cond);
                let jump = new_value!(program, scope).jump(while_cond);
                push_insts!(program, scope, jump);

                let while_body =
                    new_bb!(program, scope).basic_block(Some("%while_body".to_string()));
                add_bb_to_program!(program, scope, while_body);
                let while_end = new_bb!(program, scope).basic_block(Some("%while_end".to_string()));
                add_bb_to_program!(program, scope, while_end);

                scope.enter_loop(while_cond, while_end);
                scope.set_bb(while_cond);

                let cond = while_stmt
                    .cond
                    .generate(program, scope)?
                    .into_value(program, scope);
                let br = new_value!(program, scope).branch(cond, while_body, while_end);
                push_insts!(program, scope, br);

                scope.set_bb(while_body);
                while_stmt.body.generate(program, scope)?;
                maybe_add_jump(program, scope, while_cond);

                scope.exit_loop();
                scope.set_bb(while_end);
                Ok(())
            }
            Stmt::Break => {
                let loop_block = scope.get_loop_block()?;
                let jump = new_value!(program, scope).jump(loop_block.exit);
                push_insts!(program, scope, jump);

                let bb = new_bb!(program, scope).basic_block(Some("%after_break".to_string()));
                add_bb_to_program!(program, scope, bb);
                scope.set_bb(bb);
                Ok(())
            }
            Stmt::Continue => {
                let loop_block = scope.get_loop_block()?;
                let jump = new_value!(program, scope).jump(loop_block.entry);
                push_insts!(program, scope, jump);

                let bb = new_bb!(program, scope).basic_block(Some("%after_continue".to_string()));
                add_bb_to_program!(program, scope, bb);
                scope.set_bb(bb);
                Ok(())
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
                let or_false = new_bb!(program, scope).basic_block(Some("%lor_false".to_string()));
                add_bb_to_program!(program, scope, or_false);
                let or_end = new_bb!(program, scope).basic_block(Some("%lor_end".to_string()));
                add_bb_to_program!(program, scope, or_end);
                let zero = new_value!(program, scope).integer(0);

                let result = new_value!(program, scope).alloc(Type::get_i32());
                curr_func_mut!(program, scope)
                    .dfg_mut()
                    .set_value_name(result, Some("%logic_result".to_string()));

                let lhs = lor_exp.generate(program, scope)?.into_value(program, scope);
                let not_eq1 = new_value!(program, scope).binary(BinaryOp::NotEq, lhs, zero);
                let store_to_result = new_value!(program, scope).store(not_eq1, result);
                let br = new_value!(program, scope).branch(not_eq1, or_end, or_false);
                push_insts!(program, scope, not_eq1, result, store_to_result, br);

                scope.set_bb(or_false);
                let rhs = land_exp
                    .generate(program, scope)?
                    .into_value(program, scope);
                let not_eq2 = new_value!(program, scope).binary(BinaryOp::NotEq, rhs, zero);
                let store_to_result2 = new_value!(program, scope).store(not_eq2, result);
                let jump = new_value!(program, scope).jump(or_end);
                // TODO: stack size %logic_result
                push_insts!(program, scope, not_eq2, store_to_result2, jump);

                scope.set_bb(or_end);
                Ok(SymbolValue::NeedLoad(result))
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
                let and_true = new_bb!(program, scope).basic_block(Some("%land_true".to_string()));
                add_bb_to_program!(program, scope, and_true);
                let and_end = new_bb!(program, scope).basic_block(Some("%land_end".to_string()));
                add_bb_to_program!(program, scope, and_end);
                let zero = new_value!(program, scope).integer(0);

                let result = new_value!(program, scope).alloc(Type::get_i32());
                curr_func_mut!(program, scope)
                    .dfg_mut()
                    .set_value_name(result, Some("%logic_result".to_string()));

                let lhs = land_exp
                    .generate(program, scope)?
                    .into_value(program, scope);
                let not_eq1 = new_value!(program, scope).binary(BinaryOp::NotEq, lhs, zero);
                let store_to_result = new_value!(program, scope).store(zero, result);
                let br = new_value!(program, scope).branch(not_eq1, and_true, and_end);
                push_insts!(program, scope, not_eq1, result, store_to_result, br);

                scope.set_bb(and_true);
                let rhs = eq_exp.generate(program, scope)?.into_value(program, scope);
                let not_eq2 = new_value!(program, scope).binary(BinaryOp::NotEq, rhs, zero);
                let store_to_result2 = new_value!(program, scope).store(not_eq2, result);
                let jump = new_value!(program, scope).jump(and_end);

                push_insts!(program, scope, not_eq2, store_to_result2, jump);

                scope.set_bb(and_end);
                Ok(SymbolValue::NeedLoad(result))
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
                push_insts!(program, scope, inst);
                Ok(SymbolValue::Value(inst))
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
                push_insts!(program, scope, inst);
                Ok(SymbolValue::Value(inst))
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
                push_insts!(program, scope, inst);
                Ok(SymbolValue::Value(inst))
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
                push_insts!(program, scope, inst);
                Ok(SymbolValue::Value(inst))
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
                    push_insts!(program, scope, inst);
                    Ok(SymbolValue::Value(inst))
                }
                UnaryOp::Not => {
                    let l_value = unary_exp
                        .generate(program, scope)?
                        .into_value(program, scope);
                    let r_value = new_value!(program, scope).integer(0);
                    let inst = new_value!(program, scope).binary(BinaryOp::Eq, r_value, l_value);
                    push_insts!(program, scope, inst);
                    Ok(SymbolValue::Value(inst))
                }
            },
            UnaryExp::Call(func_call) => {
                match scope.global.function.get(func_call.ident.as_str()) {
                    Some(func) => {
                        let func = func.clone();
                        let mut args = vec![];
                        for exp in &func_call.args {
                            let arg = exp.generate(program, scope)?.into_value(program, scope);
                            args.push(arg);
                        }
                        let call = new_value!(program, scope).call(func, args);
                        push_insts!(program, scope, call);
                        Ok(SymbolValue::Value(call))
                    }
                    None => Err(Undefined(format!(
                        "Function Undefined: {:?}",
                        func_call.ident
                    ))),
                }
            }
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
            PrimaryExp::Number(n) => Ok(SymbolValue::Value(new_value!(program, scope).integer(*n))),
            PrimaryExp::LVal(lval) => match scope.get(&lval.ident) {
                Ok(v) => Ok(v),
                Err(err) => Err(err),
            },
        }
    }
}
