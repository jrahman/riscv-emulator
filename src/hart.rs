use crate::decode::{decode_I, decode_S};
use crate::memory::Memory;

/// Represents the state for a Hardware Thread in RISC-V. Includes all unprivilged architectual states
///
pub struct Hart {
    // Program Counter which indicates the memory address of the currently executing instruction
    pc: u32,

    // Array of all 32 General purpose registers. For uniformity, we include x0 in the array, but handle it specially on access
    regs: [i32; 32],
}

impl Hart {
    pub fn new() -> Self {
        Self {
            pc: 0,
            regs: [0; 32],
        }
    }

    // Execute a single instruction at the current program counter (PC) address
    pub fn execute(&mut self, memory: &mut Memory) {
        let inst = memory.load_inst(self.pc);

        // Assuming a 32-bit instruction, extract the opcode from the first 6 bits
        let opcode = inst & 63;

        match opcode {
            3 /* LOAD */ => {
                let (_, rd, width, base, offset) = decode_I(inst);
                let base = self.regs[base as usize];
                let addr = unsafe { std::mem::transmute(base + offset as i32)};
                self.regs[rd as usize] = match width {
                    0 => memory.lb(addr),
                    1 => memory.lh(addr),
                    2 => memory.lw(addr),
                    4 => memory.lbu(addr),
                    _ => panic!()
                };
            },
            35 /* STORE */ => {
                let (_, width, base, src, offset) = decode_S(inst);
                let src = self.regs[src as usize];
                let base = self.regs[base as usize];
                let addr = unsafe { std::mem::transmute(base + offset as i32)};
                match width {
                    0 => memory.sb(addr, src as i8),
                    1 => memory.sh(addr, src as i16),
                    2 => memory.sw(addr, src),
                    _ => panic!()
                }
            }
            _ => (),
        }

        // Advance PC to the next instruction we need to execute
        self.pc += 4;
    }
}
