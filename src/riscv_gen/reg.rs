pub(crate) struct RegisterManager {
    registers: Registers,
}

impl RegisterManager {
    pub fn new() -> Self {
        RegisterManager {
            registers: Registers::default(),
        }
    }
}

#[derive(Debug, Default)]
pub(crate) struct Registers {
    // x0 always zero
    // x1 return address, caller-saved
    pub ra: bool,
    // x2 stack pointer, callee-saved
    pub sp: bool,
    // x3 global pointer, N/A
    pub gp: bool,
    // x4 thread pointer, N/A
    pub tp: bool,
    // x5 alternative temp, caller
    pub t0: bool,
    // x6-7 temp, caller
    pub t1: bool,
    pub t2: bool,
    // x8 s0/fp save or frame pointer, callee
    pub fp: bool,
    // x9 s1, callee
    pub s1: bool,
    // x10, x11: return/function parameter, caller
    pub a0: bool,
    pub a1: bool,
    // x12-x17: function parameter, caller
    pub a2: bool,
    pub a3: bool,
    pub a4: bool,
    pub a5: bool,
    pub a6: bool,
    pub a7: bool,
    // x18-x27 save, callee
    pub s2: bool,
    pub s3: bool,
    pub s4: bool,
    pub s5: bool,
    pub s6: bool,
    pub s7: bool,
    pub s8: bool,
    pub s9: bool,
    pub s10: bool,
    pub s11: bool,
    // x28-x31 temp, caller
    pub t3: bool,
    pub t4: bool,
    pub t5: bool,
    pub t6: bool,
}
