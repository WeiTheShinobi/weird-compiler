use crate::riscv_gen::context::Context;
use crate::riscv_gen::inst::Inst;
use koopa::front::ast::Error;
use koopa::ir::{self, *};
use std::cmp::max;
use std::vec;
use std::{fs::File, io::Write};

macro_rules! bb_name {
    ($func_data: expr, $bb: expr) => {
        $func_data
            .dfg()
            .bb($bb)
            .name()
            .clone()
            .unwrap()
            .chars()
            .skip(1)
            .collect::<String>()
    };
}

pub struct Program {
    pub insts: Vec<Inst>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            insts: vec![],
        }
    }

    pub fn generate_on(self, mut file: File) -> Result<(), Error>{
        for inst in self.insts {
            if let Err(e) = file.write_all(inst.to_isa().as_bytes()) {panic!("{:?}", e)}
            if let Err(e) = file.write_all("\n".as_bytes()) {panic!("{:?}", e)}
        }
        Ok(())
    }

    fn push_inst(&mut self, inst: Inst) {
        self.insts.push(inst);
    }

    fn newline(&mut self) {
        self.push_inst(Inst::NewLine);
    }
}

#[derive(Clone, Debug)]
pub enum AsmValue {
    Const(i32),
    Value(String),
    Register(String),
    GlobalVar(String),
}

impl AsmValue {
    fn load_to<'a>(&self, program: &'a mut Program, reg: &'a str) -> String {
        match self {
            AsmValue::Const(int) => {
                program.push_inst(Inst::Li(reg.to_string(), *int));
                reg.to_string()
            }
            AsmValue::Value(pos) => {
                program.push_inst(Inst::Lw(reg.to_string(), pos.to_string()));
                reg.to_string()
            }
            AsmValue::Register(r) => r.to_string(),
            AsmValue::GlobalVar(label) => {
                program.push_inst(Inst::La(reg.to_string(), label.to_string()));
                program.push_inst(Inst::Lw(reg.to_string(), format!("0({})", reg)));
                reg.to_string()
            }
        }
    }
}

pub trait GenerateAsm {
    fn generate(&self, asm: &mut Program, cx: &mut Context);
}

impl GenerateAsm for ir::Program {
    fn generate(&self, program: &mut Program, cx: &mut Context) {
        for &global in self.inst_layout() {
            let global_value = self.borrow_value(global);
            let alloc_value = match &global_value.kind() {
                ValueKind::GlobalAlloc(ga) => self.borrow_value(ga.init()),
                _ => unreachable!("should only have global var"),
            };
            dbg!(&alloc_value);
            let is_zero_init = matches!(alloc_value.kind(), ValueKind::ZeroInit(_));

            let value_name = global_value.name().clone().unwrap().replace("@", "");
            if is_zero_init {
                program.push_inst(Inst::Directive("  .bss".to_string()));
            } else {
                program.push_inst(Inst::Directive("  .data".to_string()));
            }
            cx.set_symbol(global, AsmValue::GlobalVar(value_name.clone()));
            program.push_inst(Inst::Directive(format!("  .globl {}", value_name)));
            program.push_inst(Inst::Lable(format!("{}:", value_name)));
            if is_zero_init {
                program.push_inst(Inst::Directive("  .zero 4".to_string()));
            } else {
                let val = match alloc_value.kind() {
                    ValueKind::Integer(integer) => integer,
                    _ => unreachable!(),
                };
                program.push_inst(Inst::Directive(format!("  .word {}", val.value())));
            }
            program.newline();
        }
        for &func in self.func_layout() {
            let func_data = self.func(func);
            let func_name = if func_data.name().starts_with("@") {
                func_data.name().strip_prefix("@").unwrap()
            } else {
                func_data.name()
            };
            cx.function_table.insert(func, func_name.to_string());
        }
        for &func in self.func_layout() {
            let func_data = self.func(func);
            let func_name = cx.function_table.get(&func).unwrap();
            // skip buildin declare function
            if let None = func_data.layout().entry_bb() {
                continue;
            }
            program.push_inst(Inst::Directive("  .text".to_string()));
            program.push_inst(Inst::Directive(format!("  .globl {}", func_name)));
            program.push_inst(Inst::Directive(format!("{}:", func_name)));
            func_data.generate(program, cx);
            cx.clear()
        }
    }
}

impl GenerateAsm for FunctionData {
    fn generate(&self, program: &mut Program, cx: &mut Context) {
        let (stack_size, has_func_call) = calculate_stack_size(self);
        cx.stack_size = stack_size;
        if has_func_call {
            cx.ra_pos = Some(stack_size - 4);
        }

        let mut is_first_block = true;
        prologue(program, &cx);
        for (&bb, node) in self.layout().bbs() {
            if !is_first_block {
                let bb_name = bb_name!(self, bb);
                program.push_inst(Inst::Lable(format!("{}:", bb_name)));
            } else {
                is_first_block = false;
            }
            for &value in node.insts().keys() {
                emit(self, value, program, cx);
                program.newline();
            }
        }
    }
}

#[inline]
fn stack_size(ty: &Type) -> usize {
    match ty.kind() {
        TypeKind::Int32 | TypeKind::Unit => ty.size(),
        TypeKind::Array(_, _) => todo!(),
        TypeKind::Pointer(val) => val.size(),
        TypeKind::Function(_, ty) => ty.size(),
    }
}

fn calculate_stack_size(function_data: &FunctionData) -> (usize, bool) {
    let mut has_func_call = false;
    let mut max_func_args_len = 0;
    let mut size = 0;
    for (&_bb, node) in function_data.layout().bbs() {
        for &inst in node.insts().keys() {
            let value_data = function_data.dfg().value(inst);
            match value_data.kind() {
                ValueKind::Call(call) => {
                    has_func_call = true;
                    max_func_args_len = max(max_func_args_len, call.args().len());
                    size += stack_size(value_data.ty());
                }
                _ => size += stack_size(value_data.ty()),
            }
        }
    }
    if max_func_args_len > 8 {
        size += (max_func_args_len - 8) * 4;
    }
    if has_func_call {
        size += 4;
    }
    if size % 16 == 0 {
        (size, has_func_call)
    } else {
        (size + 16 - (size % 16), has_func_call)
    }
}
fn prologue(program: &mut Program, cx: &Context) {
    if cx.stack_size == 0 {
        return;
    }
    program.push_inst(Inst::Comment("# prolugue".to_string()));
    // addi -> 2^12 [-2048, 2047]
    if cx.stack_size < 2047 {
        program.push_inst(Inst::Addi(
            "sp".to_string(),
            "sp".to_string(),
            -(cx.stack_size as i32),
        ));
    } else {
        program.push_inst(Inst::Li("t0".to_string(), -(cx.stack_size as i32)));
        program.push_inst(Inst::Add(
            "sp".to_string(),
            "sp".to_string(),
            "t0".to_string(),
        ));
    }
    if let Some(ra_pos) = cx.ra_pos {
        program.push_inst(Inst::Sw("ra".to_string(), format!("{}(sp)", ra_pos)));
    }
    program.newline();
}

fn epilogue(program: &mut Program, cx: &Context) {
    if cx.stack_size == 0 {
        return;
    }
    program.push_inst(Inst::Comment("# epilogue".to_string()));

    if let Some(ra_pos) = cx.ra_pos {
        program.push_inst(Inst::Lw("ra".to_string(), format!("{}(sp)", ra_pos)));
    }
    // addi -> 2^12 [-2048, 2047]
    if cx.stack_size < 2047 {
        program.push_inst(Inst::Addi(
            "sp".to_string(),
            "sp".to_string(),
            cx.stack_size as i32,
        ));
    } else {
        program.push_inst(Inst::Li("t0".to_string(), cx.stack_size as i32));
        program.push_inst(Inst::Add(
            "sp".to_string(),
            "sp".to_string(),
            "t0".to_string(),
        ));
    }
}

fn emit(func_data: &FunctionData, value: Value, program: &mut Program, cx: &mut Context) {
    let value_data = func_data.dfg().value(value);

    dbg!(value_data);
    match value_data.kind() {
        ValueKind::Integer(int) => {
            let imm = int.value();
            cx.set_symbol(value, AsmValue::Const(imm));
        }
        ValueKind::Binary(binary) => {
            program.push_inst(Inst::Comment("# binary".to_string()));
            if let None = cx.get_symbol(&binary.lhs()) {
                emit(func_data, binary.lhs(), program, cx);
            }
            let lhs_value = cx.get_symbol(&binary.lhs()).unwrap().clone();
            lhs_value.load_to(program, "t0");
            if let None = cx.get_symbol(&binary.rhs()) {
                emit(func_data, binary.rhs(), program, cx);
            }
            let rhs_value = cx.get_symbol(&binary.rhs()).unwrap().clone();
            rhs_value.load_to(program, "t1");

            match binary.op() {
                BinaryOp::Eq => {
                    program.push_inst(Inst::Sub(
                        "t0".to_string(),
                        "t0".to_string(),
                        "t1".to_string(),
                    ));
                    program.push_inst(Inst::Seqz("t0".to_string(), "t0".to_string()));
                }
                BinaryOp::NotEq => {
                    program.push_inst(Inst::Sub(
                        "t0".to_string(),
                        "t0".to_string(),
                        "t1".to_string(),
                    ));
                    program.push_inst(Inst::Snez("t0".to_string(), "t0".to_string()));
                }
                BinaryOp::Sub => program.push_inst(Inst::Sub(
                    "t0".to_string(),
                    "t0".to_string(),
                    "t1".to_string(),
                )),
                BinaryOp::Mul => program.push_inst(Inst::Mul(
                    "t0".to_string(),
                    "t0".to_string(),
                    "t1".to_string(),
                )),
                BinaryOp::Add => program.push_inst(Inst::Add(
                    "t0".to_string(),
                    "t0".to_string(),
                    "t1".to_string(),
                )),
                BinaryOp::Div => program.push_inst(Inst::Div(
                    "t0".to_string(),
                    "t0".to_string(),
                    "t1".to_string(),
                )),
                BinaryOp::Gt => {
                    program.push_inst(Inst::Sgt(
                        "t0".to_string(),
                        "t0".to_string(),
                        "t1".to_string(),
                    ));
                    program.push_inst(Inst::Snez("t0".to_string(), "t0".to_string()));
                }
                BinaryOp::Lt => {
                    program.push_inst(Inst::Slt(
                        "t0".to_string(),
                        "t0".to_string(),
                        "t1".to_string(),
                    ));
                    program.push_inst(Inst::Snez("t0".to_string(), "t0".to_string()));
                }
                BinaryOp::Ge => {
                    program.push_inst(Inst::Slt(
                        "t0".to_string(),
                        "t0".to_string(),
                        "t1".to_string(),
                    ));
                    program.push_inst(Inst::Snez("t0".to_string(), "t0".to_string()));
                }
                BinaryOp::Le => {
                    program.push_inst(Inst::Sgt(
                        "t0".to_string(),
                        "t0".to_string(),
                        "t1".to_string(),
                    ));
                    program.push_inst(Inst::Snez("t0".to_string(), "t0".to_string()));
                }
                _ => unimplemented!("op: {}", binary.op()),
            }

            let pos = cx.get_useful_space(stack_size(value_data.ty()));
            program.push_inst(Inst::Sw("t0".to_string(), pos.clone()));
            cx.set_symbol(value, AsmValue::Value(pos));
        }
        ValueKind::Alloc(_) => {
            program.push_inst(Inst::Comment("# alloc".to_string()));
            let pos = cx.get_useful_space(stack_size(value_data.ty()));
            cx.set_symbol(value, AsmValue::Value(pos));
        }
        ValueKind::Load(load) => {
            program.push_inst(Inst::Comment("# load".to_string()));
            cx.get_symbol(&load.src()).unwrap().load_to(program, "t0");
            let pos = cx.get_useful_space(stack_size(value_data.ty()));

            program.push_inst(Inst::Sw("t0".to_string(), pos.clone()));
            cx.set_symbol(value, AsmValue::Value(pos));
        }
        ValueKind::Store(store) => {
            program.push_inst(Inst::Comment("# store".to_string()));

            if let None = cx.get_symbol(&store.value()) {
                emit(func_data, store.value(), program, cx);
            }
            let source_pos = cx
                .get_symbol(&store.value())
                .unwrap()
                .clone()
                .load_to(program, "t0");

            if let None = cx.get_symbol(&store.dest()) {
                emit(func_data, store.dest(), program, cx);
            }
            if let AsmValue::Value(dest) = cx.get_symbol(&store.dest()).unwrap().clone() {
                program.push_inst(Inst::Sw(source_pos.to_string(), dest.to_string()));
            }
        }
        ValueKind::Call(call) => {
            program.push_inst(Inst::Comment("# call".to_string()));

            for (i, arg) in call.args().iter().enumerate() {
                if let None = cx.get_symbol(arg) {
                    emit(func_data, *arg, program, cx);
                }
                if i <= 7 {
                    let pos = format!("a{}", i);
                    cx.get_symbol(arg).unwrap().load_to(program, pos.as_str());
                } else {
                    let arg_data = func_data.dfg().value(*arg);
                    let pos = cx.get_useful_space(stack_size(arg_data.ty()));
                    cx.get_symbol(arg).unwrap().load_to(program, pos.as_str());
                }
            }

            let callee = cx.function_table.get(&call.callee()).unwrap();
            program.push_inst(Inst::Call(format!("call {}", callee)));
            // save return value
            let return_val_pos = cx.get_useful_space(stack_size(&value_data.ty()));
            if !value_data.ty().is_unit() {
                program.push_inst(Inst::Sw("a0".to_string(), return_val_pos.to_string()));
            }

            cx.symbol_table
                .insert(value, AsmValue::Value(return_val_pos));
        }
        ValueKind::Return(ret) => {
            program.push_inst(Inst::Comment("# return".to_string()));
            if let Some(ret_val) = ret.value() {
                if let None = cx.get_symbol(&ret_val) {
                    emit(func_data, ret_val, program, cx);
                }
                cx.get_symbol(&ret_val).unwrap().load_to(program, "a0");
            };

            epilogue(program, cx);
            program.push_inst(Inst::Ret);
        }
        ValueKind::Branch(branch) => {
            program.push_inst(Inst::Comment("# branch".to_string()));
            if let None = cx.get_symbol(&branch.cond()) {
                emit(func_data, branch.cond(), program, cx);
            }
            let cond = cx.get_symbol(&branch.cond()).unwrap().clone();
            cond.load_to(program, "t0");

            let true_bb_name = bb_name!(func_data, branch.true_bb());
            let false_bb_name = bb_name!(func_data, branch.false_bb());
            program.push_inst(Inst::Bnez("t0".to_string(), true_bb_name));
            program.push_inst(Inst::J(false_bb_name));
        }
        ValueKind::Jump(jump) => {
            let target_bb_name = bb_name!(func_data, jump.target());
            program.push_inst(Inst::J(target_bb_name));
        }
        ValueKind::FuncArgRef(arg) => {
            program.push_inst(Inst::Comment(format!(
                "# func arg ref index: {}",
                arg.index()
            )));
            // args on reg a0 ~ a7
            // if len(args) > 8 => on stack
            // sp + 0 => 9
            // sp + n => 10 ...
            if arg.index() <= 7 {
                let pos = format!("a{}", arg.index());
                cx.set_symbol(value, AsmValue::Register(pos));
            } else {
                // TODO
            }
        }
        _ => unimplemented!("{:?}", value_data),
    }
}
