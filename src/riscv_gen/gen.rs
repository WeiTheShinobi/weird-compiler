use ir::entities::ValueData;
use koopa::*;
use std::{
    io::{self, Write},
    vec,
};

pub struct Program {
    asm: Vec<String>,
}

impl Program {
    pub fn new() -> Program {
        Program { asm: vec![] }
    }

    fn write(&mut self, insts: &mut Vec<String>) {
        self.asm.append(insts);
    }

    pub fn output<W: Write>(&self, mut writer: W) -> Result<(), io::Error> {
        for line in &self.asm {
            writer.write_all(line.as_bytes())?;
            writer.write_all(b"\n")?;
        }
        Ok(())
    }
}

pub trait GenerateAsm {
    fn generate(&self, asm: &mut Program);
}

impl GenerateAsm for ir::Program {
    fn generate(&self, asm: &mut Program) {
        asm.write(&mut vec!["  .text".to_string(), "  .global".to_string()]);
        for &func in self.func_layout() {
            let func_data = self.func(func);
            asm.write(&mut vec![format!("{}:", func_data.name())]);
            func_data.generate(asm);
        }
    }
}

impl GenerateAsm for koopa::ir::FunctionData {
    fn generate(&self, asm: &mut Program) {
        for (&bb, node) in self.layout().bbs() {
            for &inst in node.insts().keys() {
                let value_data = self.dfg().value(inst);
                let mut value = emit(self, value_data);
                asm.write(&mut value);
            }
        }
    }
}

fn emit(func: &koopa::ir::FunctionData, v: &ValueData) -> Vec<String> {
    match v.kind() {
        koopa::ir::ValueKind::Integer(v) => vec![v.value().to_string()],
        koopa::ir::ValueKind::Return(v) => {
            let value_data = func.dfg().value(v.value().unwrap());
            let return_data = emit(func, value_data);
            vec![format!("  li a0, {}", return_data[0]), "  ret".to_string()]
        }
        _ => unreachable!(),
    }
}
