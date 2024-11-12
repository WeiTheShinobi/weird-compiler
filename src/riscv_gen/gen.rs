use entities::ValueData;
use koopa::ir::{self, *};
use std::arch::asm;
use std::fmt::{format, Pointer};
use std::{collections::HashMap, fs::File, io::Write, vec};

pub struct Program {
    asm: Vec<String>,
    writer: File,
}

impl Program {
    pub fn new(file: File) -> Program {
        Program {
            asm: vec![],
            writer: file,
        }
    }

    fn write(&mut self, inst: &str) {
        self.writer.write_all(inst.as_bytes());
        self.writer.write_all("\n".as_bytes());
    }
}

#[derive(Clone, Debug)]
struct AsmValue {
    position: String,
    is_pointer: bool,
}
struct Context {
    stack_size: usize,
    stack_pointer: usize,
    symbol_table: HashMap<Value, AsmValue>,
}

impl Context {
    fn new() -> Context {
        Context {
            stack_size: 0,
            stack_pointer: 0,
            symbol_table: HashMap::new(),
        }
    }

    fn get_useful_space(&mut self, size: usize) -> String {
        let start_pos = self.stack_pointer;
        self.stack_pointer += size;

        assert!(self.stack_pointer <= self.stack_size, "{}, {}", self.stack_pointer, self.stack_size);
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

        prologue(program, &cx);
        for (&_bb, node) in self.layout().bbs() {
            for &value in node.insts().keys() {            
                emit(self, value, program, &mut cx);
                program.write("");
            }
        }
    }
}

fn calculate_stack_size(function_data: &FunctionData) -> usize {
    let mut size = 0;
    for (&_bb, node) in function_data.layout().bbs() {
        for &inst in node.insts().keys() {
            let value_data = function_data.dfg().value(inst);
            match value_data.kind() {
                ValueKind::Alloc(_) => size += value_data.ty().size(),
                ValueKind::Load(_) => size += value_data.ty().size(),
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
    program.write("  # prologue");
    // addi -> 2^12 [-2048, 2047]
    if cx.stack_size < 2047 {
        program.write(format!("  addi sp sp -{}", cx.stack_size).as_str());
    } else {
        program.write(format!("  li t0 -{}", cx.stack_size).as_str());
        program.write("  add sp sp t0");
    }
    program.write("");
}

fn epilogue(program: &mut Program, cx: &Context) {
    program.write("  # epilogue");
    // addi -> 2^12 [-2048, 2047]
    if cx.stack_size < 2047 {
        program.write(format!("  addi sp sp {}", cx.stack_size).as_str());
    } else {
        program.write(format!("  li t0 {}", cx.stack_size).as_str());
        program.write("  add sp sp t0");
    }
    program.write("");
}

fn emit(func_data: &FunctionData, value: Value, program: &mut Program, cx: &mut Context) {
    let value_data = func_data.dfg().value(value);
    
    dbg!(value_data);
    match value_data.kind() {
        ValueKind::Integer(int) => {
            let imm = int.value().to_string();
            program.write(format!("  li t0, {}", imm).as_str());
            cx.set_symbol(
                value,
                AsmValue {
                    position: "t0".to_string(),
                    is_pointer: false,
                },
            );
        }
        ValueKind::Binary(binary) => {
            program.write("  # binary");
            if let None = cx.get_symbol(&binary.lhs()) {
                emit(func_data, binary.lhs(), program, cx);
            }
            let lhs_value = cx.get_symbol(&binary.lhs()).unwrap().clone().position;
            if let None = cx.get_symbol(&binary.rhs()) {
                emit(func_data, binary.rhs(), program, cx);
            }
            let rhs_value = cx.get_symbol(&binary.rhs()).unwrap().clone().position;

            match binary.op() {
                BinaryOp::Eq => {
                    program.write(
                        format!("  sub {}, {}, {}", rhs_value, lhs_value, rhs_value).as_str(),
                    );
                    program.write(format!("  seqz {}, {}", rhs_value, rhs_value).as_str());
                }
                BinaryOp::NotEq => {
                    program.write(
                        format!("  sub {}, {}, {}", rhs_value, lhs_value, rhs_value).as_str(),
                    );
                    program.write(format!("  snez {}, {}", rhs_value, rhs_value).as_str());
                }
                BinaryOp::Sub => {
                    program.write(
                        format!("  sub {}, {}, {}", rhs_value, lhs_value, rhs_value).as_str(),
                    );
                }
                BinaryOp::Mul => {
                    program.write(
                        format!("  mul {}, {}, {}", rhs_value, lhs_value, rhs_value).as_str(),
                    );
                }
                BinaryOp::Add => {
                    program.write(
                        format!("  add {}, {}, {}", rhs_value, lhs_value, rhs_value).as_str(),
                    );
                }
                BinaryOp::Gt => {
                    program.write(
                        format!("  sgt {}, {}, {}", rhs_value, lhs_value, rhs_value).as_str(),
                    );
                    program.write(format!("  seqz {}, {}", rhs_value, rhs_value).as_str());
                }
                BinaryOp::Lt => {
                    program.write(
                        format!("  slt {}, {}, {}", rhs_value, lhs_value, rhs_value).as_str(),
                    );
                    program.write(format!("  seqz {}, {}", rhs_value, rhs_value).as_str());
                }
                BinaryOp::Ge => {
                    program.write(
                        format!("  slt {}, {}, {}", rhs_value, lhs_value, rhs_value).as_str(),
                    );
                    program.write(format!("  seqz {}, {}", rhs_value, rhs_value).as_str());
                }
                BinaryOp::Le => {
                    program.write(
                        format!("  sgt {}, {}, {}", rhs_value, lhs_value, rhs_value).as_str(),
                    );
                    program.write(format!("  seqz {}, {}", rhs_value, rhs_value).as_str());
                }
                _ => unimplemented!("op: {}", binary.op()),
            }
            cx.set_symbol(value, AsmValue{
                position: "t0".to_string(),
                is_pointer: false,
            });
        }
        ValueKind::Alloc(alloc) => {
            program.write("  # alloc");
            let pos = cx.get_useful_space(value_data.ty().size());
            cx.symbol_table.insert(
                value,
                AsmValue {
                    position: pos,
                    is_pointer: true,
                },
            );
        }
        ValueKind::Load(load) => {
            program.write("  # load");
            let load = cx.symbol_table.get(&load.src()).unwrap();
            program.write(format!("  lw t0, {}", load.position).as_str());
            program.write(format!("  sw t0, {}(sp) ", cx.stack_pointer).as_str());
            cx.stack_pointer += value_data.ty().size();
        }
        ValueKind::Store(store) => {
            program.write("  # store");
            if let None = cx.get_symbol(&store.value()) {
                emit(func_data, store.value(), program, cx);
            }
            let value = cx.get_symbol(&store.value()).unwrap().clone();
            if let None = cx.get_symbol(&store.dest()) {
                emit(func_data, store.dest(), program, cx);
            }
            let dest = cx.get_symbol(&store.dest()).unwrap().clone();

            program.write(format!("  lw t0, {}", value.position).as_str());
            program.write(format!("  sw t0, {}", dest.position).as_str());
        }
        ValueKind::Return(ret) => {
            if let Some(ret_val) = ret.value() {
                let ret_val_data = cx.get_symbol(&ret_val).unwrap();
                program.write(format!("lw a0, {}", ret_val_data.position).as_str());
            }
            epilogue(program, cx);
            program.write("  ret");
        }
        _ => unimplemented!("{:?}", value_data),
    }
}
