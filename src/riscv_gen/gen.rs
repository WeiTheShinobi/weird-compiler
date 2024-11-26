use koopa::ir::{self, *};
use std::{collections::HashMap, fs::File, io::Write, mem::transmute, vec};
use std::fmt::format;

macro_rules! write_inst {
    ($program:expr, $action: expr, $($values:expr), *) => {
        $program.write(
            format!(
                "  {} {}",
                $action,
                vec![$(format!("{}", $values)),+].join(", ")
            )
            .as_str(),
        )
    };
    ($program:expr, $action: expr) => {
        $program.write(
            format!(
                "  {}",
                $action
            )
            .as_str(),
        )
    };
}

macro_rules! bb_name {
    ($func_data: expr, $bb: expr) => {
        $func_data.dfg().bb($bb).name().clone().unwrap().chars().skip(1).collect::<String>()
    };
}

pub struct Program {
    writer: File,
}

impl Program {
    pub fn new(file: File) -> Program {
        Program {
            writer: file,
        }
    }

    #[rustfmt::skip]
    fn write(&mut self, inst: &str) {
        if let Err(e) = self.writer.write_all(inst.as_bytes()) {panic!("{:?}", e)}
        if let Err(e) = self.writer.write_all("\n".as_bytes()) {panic!("{:?}", e)}
    }

    fn newline(&mut self) {
        self.write("");
    }
}

#[derive(Clone, Debug)]
enum AsmValue {
    Const(i32),
    Value(String),
}

impl AsmValue {
    fn load_to(&self, program: &mut Program, reg: &str) {
        match self {
            AsmValue::Const(int) => write_inst!(program, "li", reg, int),
            AsmValue::Value(pos) => write_inst!(program, "lw", reg, pos),
        }
    }
}
struct Context {
    stack_size: usize,
    stack_used_size: usize,
    symbol_table: HashMap<Value, AsmValue>,
}

impl Context {
    fn new() -> Context {
        Context {
            stack_size: 0,
            stack_used_size: 0,
            symbol_table: HashMap::new(),
        }
    }

    fn get_useful_space(&mut self, size: usize) -> String {
        let start_pos = self.stack_used_size;
        self.stack_used_size += size;

        assert!(
            self.stack_used_size <= self.stack_size,
            "{}, {}",
            self.stack_used_size,
            self.stack_size
        );
        format!("{}(sp)", start_pos)
    }

    fn get_symbol(&self, key: &Value) -> Option<&AsmValue> {
        self.symbol_table.get(key)
    }

    fn set_symbol(&mut self, k: Value, v: AsmValue) {
        self.symbol_table.insert(k, v);
    }
}

pub trait GenerateAsm {
    fn generate(&self, asm: &mut Program);
}

impl GenerateAsm for ir::Program {
    fn generate(&self, program: &mut Program) {
        program.write("  .text");
        program.write("  .globl main");
        for &func in self.func_layout() {
            let func_data = self.func(func);
            let func_name = if func_data.name().starts_with("@") {
                func_data.name().strip_prefix("@").unwrap()
            } else {
                func_data.name()
            };
            program.write(format!("{}:", func_name).as_str());
            func_data.generate(program);
        }
    }
}

impl GenerateAsm for FunctionData {
    fn generate(&self, program: &mut Program) {
        let mut cx = Context::new();
        let stack_size = calculate_stack_size(self);
        cx.stack_size = stack_size;

        let mut is_first_block = true;
        prologue(program, &cx);
        for (&bb, node) in self.layout().bbs() {
            if !is_first_block {
                let bb_name = bb_name!(self, bb);
                program.write(format!("{}:", bb_name).as_str());
            } else {
                is_first_block = false;
            }
            for &value in node.insts().keys() {
                emit(self, value, program, &mut cx);
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
        TypeKind::Function(_, _) => todo!(),
    }
}

fn calculate_stack_size(function_data: &FunctionData) -> usize {
    let mut size = 0;
    for (&_bb, node) in function_data.layout().bbs() {
        for &inst in node.insts().keys() {
            let value_data = function_data.dfg().value(inst);
            match value_data.kind() {
                ValueKind::Alloc(_) => size += stack_size(value_data.ty()),
                ValueKind::Load(_) => size += stack_size(value_data.ty()),
                ValueKind::Binary(_) => size += stack_size(value_data.ty()),
                _ => (),
            }
        }
    }
    if size % 16 == 0 {
        size
    } else {
        size + 16 - (size % 16)
    }
}
fn prologue(program: &mut Program, cx: &Context) {
    if cx.stack_size == 0 {
        return;
    }
    write_inst!(program, "# prolugue");
    // addi -> 2^12 [-2048, 2047]
    if cx.stack_size < 2047 {
        program.write(format!("  addi sp, sp, -{}", cx.stack_size).as_str());
    } else {
        program.write(format!("  li t0, -{}", cx.stack_size).as_str());
        program.write("  add sp, sp, t0");
    }
    program.newline();
}

fn epilogue(program: &mut Program, cx: &Context) {
    if cx.stack_size == 0 {
        return;
    }
    program.write("  # epilogue");
    // addi -> 2^12 [-2048, 2047]
    if cx.stack_size < 2047 {
        write_inst!(program, "addi", "sp", "sp", cx.stack_size);
    } else {
        write_inst!(program, "li", "t0", cx.stack_size);
        write_inst!(program, "add", "sp", "sp", "t0");
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
            program.write("  # binary");
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
                    write_inst!(program, "sub", "t0", "t0", "t1");
                    write_inst!(program, "seqz", "t0", "t0");
                }
                BinaryOp::NotEq => {
                    write_inst!(program, "sub", "t0", "t0", "t1");
                    write_inst!(program, "snez", "t0", "t0");
                }
                BinaryOp::Sub => write_inst!(program, "sub", "t0", "t0", "t1"),
                BinaryOp::Mul => write_inst!(program, "mul", "t0", "t0", "t1"),
                BinaryOp::Add => write_inst!(program, "add", "t0", "t0", "t1"),
                BinaryOp::Gt => {
                    write_inst!(program, "sgt", "t0", "t0", "t1");
                    write_inst!(program, "snez", "t0", "t0");
                }
                BinaryOp::Lt => {
                    write_inst!(program, "slt", "t0", "t0", "t1");
                    write_inst!(program, "snez", "t0", "t0");
                }
                BinaryOp::Ge => {
                    write_inst!(program, "slt", "t0", "t0", "t1");
                    write_inst!(program, "snez", "t0", "t0");
                }
                BinaryOp::Le => {
                    write_inst!(program, "sgt", "t0", "t0", "t1");
                    write_inst!(program, "snez", "t0", "t0");
                }
                _ => unimplemented!("op: {}", binary.op()),
            }

            let pos = cx.get_useful_space(stack_size(value_data.ty()));
            program.write(format!("  sw t0, {}", pos).as_str());
            cx.set_symbol(value, AsmValue::Value(pos));
        }
        ValueKind::Alloc(_) => {
            write_inst!(program, "# alloc");
            let pos = cx.get_useful_space(stack_size(value_data.ty()));
            cx.set_symbol(value, AsmValue::Value(pos));
        }
        ValueKind::Load(load) => {
            write_inst!(program, "# load");
            cx.get_symbol(&load.src()).unwrap().load_to(program, "t0");
            let pos = cx.get_useful_space(stack_size(value_data.ty()));

            write_inst!(program, "sw", "t0", &pos);
            cx.set_symbol(value, AsmValue::Value(pos));
        }
        ValueKind::Store(store) => {
            write_inst!(program, "# store");

            if let None = cx.get_symbol(&store.value()) {
                emit(func_data, store.value(), program, cx);
            }
            cx.get_symbol(&store.value())
                .unwrap()
                .clone()
                .load_to(program, "t0");

            if let None = cx.get_symbol(&store.dest()) {
                emit(func_data, store.dest(), program, cx);
            }
            if let AsmValue::Value(dest) = cx.get_symbol(&store.dest()).unwrap().clone() {
                write_inst!(program, "sw", "t0", dest);
            }
        }
        ValueKind::Return(ret) => {
            write_inst!(program, "# return");
            if let Some(ret_val) = ret.value() {
                if let None = cx.get_symbol(&ret_val) {
                    emit(func_data, ret_val, program, cx);
                }
                cx.get_symbol(&ret_val).unwrap().load_to(program, "a0")
            }
            epilogue(program, cx);
            write_inst!(program, "ret");
        }
        ValueKind::Branch(branch) => {
            write_inst!(program ,"# branch");
            if let None = cx.get_symbol(&branch.cond()) {
                emit(func_data, branch.cond(), program, cx);
            }
            let cond = cx.get_symbol(&branch.cond()).unwrap().clone();
            cond.load_to(program, "t0");

            let true_bb_name = bb_name!(func_data, branch.true_bb());
            let false_bb_name = bb_name!(func_data, branch.false_bb());
            write_inst!(program, "bnez", "t0", true_bb_name);
            write_inst!(program, "j", false_bb_name);
        }
        ValueKind::Jump(jump) => {
            let target_bb_name = bb_name!(func_data, jump.target());
            write_inst!(program, "j", target_bb_name);
        },
        ValueKind::FuncArgRef(arg) => {
            // args on reg a0 ~ a7
            // if len(args) > 8 => on stack
            // sp + 0 => 9
            // sp + n => 10 ...
            if arg.index() <= 7 {
                let pos = format!("a{}", arg.index());
                cx.set_symbol(value, AsmValue::Value(pos));
                // write_inst!(program, )
            } else {

            }
        },
        _ => unimplemented!("{:?}", value_data),
    }
}
