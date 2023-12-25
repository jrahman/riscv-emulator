use crate::decode::{decode_I, decode_S, decode_U, decode_J, decode_B};
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

        let mut next_pc = self.pc + 4;

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
                self.regs[0] = 0;
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
            },
            55 /* LUI */ => {
                let (_, dst, imm) = decode_U(inst);
                // Lower 12 bits of IMM are already set to 0 from decoding
                self.regs[dst as usize] = imm;
                self.regs[0] = 0;
            },
            23 /* AUIPC */ => {
                let (_, dst, imm) = decode_U(inst);
                let value = self.pc + imm as u32;
                self.regs[dst as usize] = value as i32;
                self.regs[0] = 0;
            },
            99 /* BRANCH */ => {
                let (_, rs1, rs2, cmp, offset) = decode_B(inst);
                let lhs = self.regs[rs1 as usize];
                let rhs = self.regs[rs2 as usize];
                if match cmp {
                    0 => lhs == rhs,
                    1 => lhs != rhs,
                    4 => lhs < rhs,
                    5 => lhs >= rhs,
                    6 => (lhs as u32) < rhs as u32,
                    7 => lhs as u32 >= rhs as u32,
                    _ => panic!()
                } {
                    // TODO Address alignment check
                    next_pc = (self.pc as i32 + offset as i32) as u32;
                }
            },
            103 /* JALR */ => {
                let (_, dst, target, _, offset) = decode_I(inst);
                self.regs[dst as usize] = next_pc as i32;
                let target = self.regs[target as usize] + offset as i32;
                next_pc = target as u32 & !1;
            },
            111 /* JAL */ => {
                let (_, dst, offset) = decode_J(inst);
                self.regs[dst as usize] = next_pc as i32;
                next_pc = (self.pc as i32 + offset) as u32;
                // TODO Address alignment check
                self.regs[0] = 0;
            }
            _ => (),
        }

        // Advance PC to the next instruction we need to execute
        self.pc = next_pc;
    }
}
