use crate::decode::{decode_b, decode_i, decode_j, decode_r, decode_s, decode_u};
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
                let (_, rd, width, base, offset) = decode_i(inst);
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
            19 /* ALU Reg-Imm */ => {
                let (_, dst, src, funct3, imm) = decode_i(inst);
                let src = self.regs[src as usize];
                self.regs[dst as usize ] = match funct3 {
                    0 /* ADDI */ => {
                        src + imm as i32
                    },
                    1 /* SLLI */ => {
                        src << (imm as i32 & 31)
                    },
                    2 /* SLTI */ => {
                        if src < imm as i32 { 1 } else { 0 }
                    },
                    3 /* SLTIU */ => {
                        if (src as u32) < imm as u32 { 1 } else { 0 }
                    },
                    4 /* XORI */ => {
                        src ^ imm as i32
                    },
                    6 /* ORI */ => {
                        src | imm as i32
                    },
                    7 /* ANDI */ => {
                        src & imm as i32
                    },
                    5 /* SR*I */ => {
                        if imm >> 5 == 64 {
                            // Arithmetic right shift
                            src >> (imm as i32 & 31)
                        } else {
                            // Logical right shift
                            (src as u32 >> (imm as i32 & 31)) as i32
                        }
                    },
                    _ => panic!()
                };
                self.regs[0] = 0;
            },
            23 /* AUIPC */ => {
                let (_, dst, imm) = decode_u(inst);
                let value = self.pc + imm as u32;
                self.regs[dst as usize] = value as i32;
                self.regs[0] = 0;
            },
            35 /* STORE */ => {
                let (_, width, base, src, offset) = decode_s(inst);
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
            51 /* ALU Reg-Reg */ => {
                let (_, dst, src1, src2, funct3, funct7) = decode_r(inst);
                let src1 = self.regs[src1 as usize];
                let src2 = self.regs[src2 as usize];
                self.regs[dst as usize] = match funct3 {
                    0 /* ADD/SUB */ => {
                        if funct7 == 0 {
                            src1 + src2
                        } else {
                            src1 - src2
                        }
                    },
                    1 /* SLL */ => {
                        src1 << (src2 & 31)
                    },
                    2 /* SLT */ => {
                        if src1 < src2 { 1 } else { 0 }
                    },
                    3 /* SLTU */ => {
                        if (src1 as u32) < src2 as u32 { 1 } else { 0 }
                    },
                    4 /* XOR */ => {
                        src1 ^ src2
                    },
                    5 /* SR* */ => {
                        if funct7 == 0 {
                            ((src1 as u32) >> (src2 & 31)) as i32
                        } else {
                            src1 >> (src2 & 31)
                        }
                    },
                    6 /* OR */=> {
                        src1 | src2
                    },
                    7 /* AND */ => {
                        src1 & src2
                    },
                    _ => panic!()
                };
                self.regs[0] = 0;
            },
            55 /* LUI */ => {
                let (_, dst, imm) = decode_u(inst);
                // Lower 12 bits of IMM are already set to 0 from decoding
                self.regs[dst as usize] = imm;
                self.regs[0] = 0;
            },
            99 /* BRANCH */ => {
                let (_, rs1, rs2, cmp, offset) = decode_b(inst);
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
                let (_, dst, target, _, offset) = decode_i(inst);
                self.regs[dst as usize] = next_pc as i32;
                let target = self.regs[target as usize] + offset as i32;
                next_pc = target as u32 & !1;
            },
            111 /* JAL */ => {
                let (_, dst, offset) = decode_j(inst);
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
