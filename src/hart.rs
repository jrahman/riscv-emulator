use crate::assembly::assembler::OpCode;
use crate::decode::{decode_b, decode_i, decode_j, decode_r, decode_s, decode_u};
use crate::memory::Memory;

/// Represents the state for a Hardware Thread in RISC-V. Includes all unprivilged architectual states
///
pub struct Hart {
    // Program Counter which indicates the memory address of the currently executing instruction
    pc: u32,

    // Array of all 32 General purpose registers. For uniformity, we include x0 in the array, but handle it specially on access
    regs: [i32; 32],

    running: bool,
}

impl Hart {
    pub fn new() -> Self {
        Self {
            pc: 0,
            regs: [0; 32],
            running: true,
        }
    }

    pub fn is_running(&self) -> bool {
        self.running
    }

    pub fn set_pc(&mut self, pc: u32) {
        self.pc = pc;
    }

    pub fn get_reg(&self, reg: u8) -> i32 {
        self.regs[reg as usize]
    }

    // Execute a single instruction at the current program counter (PC) address
    pub fn execute(&mut self, memory: &mut Memory) {
        let inst = memory.load_inst(self.pc);

        // Assuming a 32-bit instruction, extract the opcode from the first 6 bits
        let opcode = OpCode::from((inst & 127) as u8);

        #[cfg(test)]
        println!(
            "Running next instruction, pc: {}, inst: {}, opcode: {:?}",
            self.pc, inst, opcode
        );

        let mut next_pc = self.pc + 4;

        match opcode {
            OpCode::LOAD /* LOAD */ => {
                let (_, rd, base, width, offset) = decode_i(inst);
                let base = self.regs[base as usize];
                let addr = unsafe { std::mem::transmute(base + offset as i32)};
                self.regs[rd as usize] = match width {
                    0 => memory.lb(addr),
                    1 => memory.lh(addr),
                    2 => memory.lw(addr),
                    4 => memory.lbu(addr),
                    5 => memory.lhu(addr),
                    _ => panic!()
                };
                self.regs[0] = 0;
            },
            OpCode::REG_IMM /* ALU Reg-Imm */ => {
                let (_, rd, src, funct3, imm) = decode_i(inst);
                let src = self.regs[src as usize];
                self.regs[rd as usize ] = match funct3 {
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
            OpCode::AUIPC /* AUIPC */ => {
                let (_, rd, imm) = decode_u(inst);
                let value = self.pc + imm as u32;
                self.regs[rd as usize] = value as i32;
                self.regs[0] = 0;
            },
            OpCode::STORE /* STORE */ => {
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
            OpCode::REG_REG /* ALU Reg-Reg */ => {
                let (_, rd, rs1, rs2, funct3, funct7) = decode_r(inst);
                let rs1 = self.regs[rs1 as usize];
                let rs2 = self.regs[rs2 as usize];
                self.regs[rd as usize] = match funct3 {
                    0 /* ADD/SUB */ => {
                        if funct7 == 0 {
                            rs1 + rs2
                        } else {
                            rs1 - rs2
                        }
                    },
                    1 /* SLL */ => {
                        rs1 << (rs2 & 31)
                    },
                    2 /* SLT */ => {
                        if rs1 < rs2 { 1 } else { 0 }
                    },
                    3 /* SLTU */ => {
                        if (rs1 as u32) < rs2 as u32 { 1 } else { 0 }
                    },
                    4 /* XOR */ => {
                        rs1 ^ rs2
                    },
                    5 /* SR* */ => {
                        if funct7 == 0 {
                            ((rs1 as u32) >> (rs2 & 31)) as i32
                        } else {
                            rs1 >> (rs2 & 31)
                        }
                    },
                    6 /* OR */=> {
                        rs1 | rs2
                    },
                    7 /* AND */ => {
                        rs1 & rs2
                    },
                    _ => panic!()
                };
                self.regs[0] = 0;
            },
            OpCode::LUI /* LUI */ => {
                let (_, rd, imm) = decode_u(inst);
                // Lower 12 bits of IMM are already set to 0 from decoding
                self.regs[rd as usize] = imm;
                self.regs[0] = 0;
            },
            OpCode::BRANCH /* BRANCH */ => {
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
            OpCode::JALR /* JALR */ => {
                let (_, rd, target, _, offset) = decode_i(inst);
                self.regs[rd as usize] = next_pc as i32;
                let target = self.regs[target as usize] + offset as i32;
                next_pc = target as u32 & !1;
            },
            OpCode::JAL /* JAL */ => {
                let (_, rd, offset) = decode_j(inst);
                self.regs[rd as usize] = next_pc as i32;
                next_pc = (self.pc as i32 + offset) as u32;
                // TODO Address alignment check
                self.regs[0] = 0;
            },
            OpCode::SYSTEM => {
                let (_, _, _, _, imm) = decode_i(inst);
                match imm {
                    // EBREAK
                    0b000000000001 => {
                        self.running = false;
                    },
                    _ => {
                        panic!()
                    }
                }
            }
            _ => (),
        }

        // Advance PC to the next instruction we need to execute
        self.pc = next_pc;
    }
}

#[cfg(test)]
mod test {
    use crate::{
        assembly::assembler::AluOps,
        decode::{encode_b, encode_i, encode_j, encode_r},
        memory::Memory,
    };

    use super::{Hart, OpCode};

    #[test]
    fn jal() {
        let mut memory = Memory::new();
        let mut hart = Hart::new();

        for dest in 1..32 {
            for offset in (-1024..1024).filter(|v| v & 1 == 0) {
                let inst = encode_j(OpCode::JAL as u8, dest, offset);

                memory.sw(1024, inst as i32);
                hart.pc = 1024;

                hart.execute(&mut memory);

                assert_eq!(hart.pc, (1024 + offset) as u32);
                assert_eq!(hart.regs[dest as usize], 1028);
            }
        }
    }

    #[test]
    fn jalr() {
        let mut memory = Memory::new();
        let mut hart = Hart::new();

        for dest in 1..32 {
            for src in (1..32).filter(|v| *v != dest) {
                for target in (0..8i32 * 1024).filter(|v| v & 3 == 0) {
                    for offset in [
                        -2048, -1167, -1024, -5, -4, 0, 1, 2, 3, 4, 16, 17, 1024, 2047,
                    ]
                    .iter()
                    .filter(|v| **v >= target)
                    {
                        let inst = encode_i(OpCode::JALR as u8, dest, src, 0, *offset as i16);

                        memory.sw(1024, inst as i32);
                        hart.pc = 1024;
                        hart.regs[src as usize] = target;

                        hart.execute(&mut memory);

                        assert_eq!(hart.pc, (target + offset) as u32 & !1u32);
                        assert_eq!(hart.regs[dest as usize], 1028);
                    }
                }
            }
        }
    }

    #[test]
    fn beq() {
        let mut memory = Memory::new();
        let mut hart = Hart::new();

        for rs1 in 1..32 {
            for rs2 in (1..32).filter(|v| *v != rs1) {
                for offset in [-4096, -2048, -1024, -1024, -510, -4, 0, 4, 124, 1024, 2048] {
                    for (lhs, rhs) in [
                        (0, 1),
                        (-1, 1),
                        (1, 1),
                        (-1, -1),
                        (0, 0),
                        (1, -1),
                        (2, -1),
                        (i32::MAX, i32::MAX),
                        (i32::MIN, i32::MIN),
                        (i32::MIN, i32::MAX),
                        (i32::MAX, i32::MIN),
                    ] {
                        let inst =
                            encode_b(OpCode::BRANCH as u8, rs1, rs2, 0b000 /* BEQ */, offset);
                        memory.sw(4096, inst as i32);
                        hart.pc = 4096;

                        hart.regs[rs1 as usize] = lhs;
                        hart.regs[rs2 as usize] = rhs;

                        hart.execute(&mut memory);

                        assert_eq!(hart.regs[rs1 as usize], lhs);
                        assert_eq!(hart.regs[rs2 as usize], rhs);
                        assert_eq!(
                            hart.pc,
                            if lhs == rhs {
                                (4096 + offset) as u32
                            } else {
                                4100
                            },
                            "Failed with: {} ?= {} -> {}",
                            lhs,
                            rhs,
                            offset
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn bne() {
        let mut memory = Memory::new();
        let mut hart = Hart::new();

        for rs1 in 1..32 {
            for rs2 in (1..32).filter(|v| *v != rs1) {
                for offset in [-4096, -2048, -1024, -1024, -510, -4, 0, 4, 124, 1024, 2048] {
                    for (lhs, rhs) in [
                        (0, 1),
                        (-1, 1),
                        (1, 1),
                        (-1, -1),
                        (0, 0),
                        (1, -1),
                        (2, -1),
                        (i32::MAX, i32::MAX),
                        (i32::MIN, i32::MIN),
                        (i32::MIN, i32::MAX),
                        (i32::MAX, i32::MIN),
                    ] {
                        let inst =
                            encode_b(OpCode::BRANCH as u8, rs1, rs2, 0b001 /* BNE */, offset);
                        memory.sw(4096, inst as i32);
                        hart.pc = 4096;

                        hart.regs[rs1 as usize] = lhs;
                        hart.regs[rs2 as usize] = rhs;

                        hart.execute(&mut memory);

                        assert_eq!(hart.regs[rs1 as usize], lhs);
                        assert_eq!(hart.regs[rs2 as usize], rhs);
                        assert_eq!(
                            hart.pc,
                            if lhs != rhs {
                                (4096 + offset) as u32
                            } else {
                                4100
                            },
                            "Failed with: {} ?= {} -> {}",
                            lhs,
                            rhs,
                            offset
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn blt() {
        let mut memory = Memory::new();
        let mut hart = Hart::new();

        for rs1 in 1..32 {
            for rs2 in (1..32).filter(|v| *v != rs1) {
                for offset in [-4096, -2048, -1024, -1024, -510, -4, 0, 4, 124, 1024, 2048] {
                    for (lhs, rhs) in [
                        (0, 1),
                        (-1, 1),
                        (1, 1),
                        (-1, -1),
                        (0, 0),
                        (1, -1),
                        (2, -1),
                        (i32::MAX, i32::MAX),
                        (i32::MIN, i32::MIN),
                        (i32::MIN, i32::MAX),
                        (i32::MAX, i32::MIN),
                    ] {
                        let inst =
                            encode_b(OpCode::BRANCH as u8, rs1, rs2, 0b100 /* BLT */, offset);
                        memory.sw(4096, inst as i32);
                        hart.pc = 4096;

                        hart.regs[rs1 as usize] = lhs;
                        hart.regs[rs2 as usize] = rhs;

                        hart.execute(&mut memory);

                        assert_eq!(hart.regs[rs1 as usize], lhs);
                        assert_eq!(hart.regs[rs2 as usize], rhs);
                        assert_eq!(
                            hart.pc,
                            if lhs < rhs {
                                (4096 + offset) as u32
                            } else {
                                4100
                            },
                            "Failed with: {} ?= {} -> {}",
                            lhs,
                            rhs,
                            offset
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn bge() {
        let mut memory = Memory::new();
        let mut hart = Hart::new();

        for rs1 in 1..32 {
            for rs2 in (1..32).filter(|v| *v != rs1) {
                for offset in [-4096, -2048, -1024, -1024, -510, -4, 0, 4, 124, 1024, 2048] {
                    for (lhs, rhs) in [
                        (0, 1),
                        (-1, 1),
                        (1, 1),
                        (-1, -1),
                        (0, 0),
                        (1, -1),
                        (2, -1),
                        (i32::MAX, i32::MAX),
                        (i32::MIN, i32::MIN),
                        (i32::MIN, i32::MAX),
                        (i32::MAX, i32::MIN),
                    ] {
                        let inst =
                            encode_b(OpCode::BRANCH as u8, rs1, rs2, 0b101 /* BGE */, offset);
                        memory.sw(4096, inst as i32);
                        hart.pc = 4096;

                        hart.regs[rs1 as usize] = lhs;
                        hart.regs[rs2 as usize] = rhs;

                        hart.execute(&mut memory);

                        assert_eq!(hart.regs[rs1 as usize], lhs);
                        assert_eq!(hart.regs[rs2 as usize], rhs);
                        assert_eq!(
                            hart.pc,
                            if lhs >= rhs {
                                (4096 + offset) as u32
                            } else {
                                4100
                            },
                            "Failed with: {} ?= {} -> {}",
                            lhs,
                            rhs,
                            offset
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn bltu() {
        let mut memory = Memory::new();
        let mut hart = Hart::new();

        for rs1 in 1..32 {
            for rs2 in (1..32).filter(|v| *v != rs1) {
                for offset in [-4096, -2048, -1024, -1024, -510, -4, 0, 4, 124, 1024, 2048] {
                    for (lhs, rhs) in [
                        (0, 1),
                        (-1, 1),
                        (1, 1),
                        (-1, -1),
                        (0, 0),
                        (1, -1),
                        (2, -1),
                        (i32::MAX, i32::MAX),
                        (i32::MIN, i32::MIN),
                        (i32::MIN, i32::MAX),
                        (i32::MAX, i32::MIN),
                    ] {
                        let inst = encode_b(
                            OpCode::BRANCH as u8,
                            rs1,
                            rs2,
                            0b110, /* BLTU */
                            offset,
                        );
                        memory.sw(4096, inst as i32);
                        hart.pc = 4096;

                        hart.regs[rs1 as usize] = lhs;
                        hart.regs[rs2 as usize] = rhs;

                        hart.execute(&mut memory);

                        assert_eq!(hart.regs[rs1 as usize], lhs);
                        assert_eq!(hart.regs[rs2 as usize], rhs);
                        assert_eq!(
                            hart.pc,
                            if (lhs as u32) < rhs as u32 {
                                (4096 + offset) as u32
                            } else {
                                4100
                            },
                            "Failed with: {} ?= {} -> {}",
                            lhs,
                            rhs,
                            offset
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn bgeu() {
        let mut memory = Memory::new();
        let mut hart = Hart::new();

        for rs1 in 1..32 {
            for rs2 in (1..32).filter(|v| *v != rs1) {
                for offset in [-4096, -2048, -1024, -1024, -510, -4, 0, 4, 124, 1024, 2048] {
                    for (lhs, rhs) in [
                        (0, 1),
                        (-1, 1),
                        (1, 1),
                        (-1, -1),
                        (0, 0),
                        (1, -1),
                        (2, -1),
                        (i32::MAX, i32::MAX),
                        (i32::MIN, i32::MIN),
                        (i32::MIN, i32::MAX),
                        (i32::MAX, i32::MIN),
                    ] {
                        let inst = encode_b(
                            OpCode::BRANCH as u8,
                            rs1,
                            rs2,
                            0b111, /* BGEU */
                            offset,
                        );
                        memory.sw(4096, inst as i32);
                        hart.pc = 4096;

                        hart.regs[rs1 as usize] = lhs;
                        hart.regs[rs2 as usize] = rhs;

                        hart.execute(&mut memory);

                        assert_eq!(hart.regs[rs1 as usize], lhs);
                        assert_eq!(hart.regs[rs2 as usize], rhs);
                        assert_eq!(
                            hart.pc,
                            if (lhs as u32) >= rhs as u32 {
                                (4096 + offset) as u32
                            } else {
                                4100
                            },
                            "Failed with: {} ?= {} -> {}",
                            lhs,
                            rhs,
                            offset
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn load() {
        let mut memory = Memory::new();
        let mut hart = Hart::new();

        for rd in 1..31 {
            for rs1 in (1..31).filter(|v| *v != rd) {
                for offset in -2048..2047 {
                    // Load Byte
                    let inst = encode_i(OpCode::LOAD as u8, rd, rs1, 0b000, offset);
                    memory.sw(16 * 1024, inst as i32);
                    memory.sb((4096 + offset) as u32, (rd + rs1) as i8);
                    hart.regs[rs1 as usize] = 4096;
                    hart.pc = 16 * 1024;

                    hart.execute(&mut memory);

                    assert_eq!(rd + rs1, hart.regs[rd as usize] as u8);
                    assert_eq!(hart.regs[rs1 as usize], 4096);

                    // Load Word
                    let inst = encode_i(OpCode::LOAD as u8, rd, rs1, 0b010, offset);
                    memory.sw(16 * 1024, inst as i32);
                    memory.sh((4096 + offset) as u32, (rd + rs1) as i16);
                    hart.regs[rs1 as usize] = 4096;
                    hart.pc = 16 * 1024;

                    hart.execute(&mut memory);

                    assert_eq!(rd + rs1, hart.regs[rd as usize] as u8);
                    assert_eq!(hart.regs[rs1 as usize], 4096);

                    // Load Half-word
                    let inst = encode_i(OpCode::LOAD as u8, rd, rs1, 0b001, offset);
                    memory.sw(16 * 1024, inst as i32);
                    memory.sw((4096 + offset) as u32, (rd + rs1) as i32);
                    hart.regs[rs1 as usize] = 4096;
                    hart.pc = 16 * 1024;

                    hart.execute(&mut memory);

                    assert_eq!(rd + rs1, hart.regs[rd as usize] as u8);
                    assert_eq!(hart.regs[rs1 as usize], 4096);
                }
            }
        }
    }

    #[test]
    fn slt_reg_imm() {
        let mut memory = Memory::new();
        let mut hart = Hart::new();

        for src in 1..32 {
            for dest in (1..32).filter(|v| *v != src) {
                for (reg, imm) in [(0, 100), (-100, 0), (0, 0), (1, 2), (2, 1), (-100, 100)] {
                    let inst = encode_i(OpCode::REG_IMM as u8, dest, src, AluOps::SLT as u8, imm);

                    memory.sw(128, inst as i32);
                    hart.pc = 128;

                    hart.regs[dest as usize] = 2;
                    hart.regs[src as usize] = reg;

                    hart.execute(&mut memory);

                    assert_eq!(reg, hart.regs[src as usize]);
                    assert_eq!(
                        hart.regs[dest as usize],
                        if reg < imm.into() { 1 } else { 0 }
                    );
                }
            }
        }
    }

    #[test]
    fn sltu_reg_imm() {
        let mut memory = Memory::new();
        let mut hart = Hart::new();

        for src in 1..32 {
            for dest in (1..32).filter(|v| *v != src) {
                for (reg, imm) in [(0, 100), (-100, 0), (0, 0), (1, 2), (2, 1), (-100, 100)] {
                    let inst = encode_i(OpCode::REG_IMM as u8, dest, src, AluOps::SLTU as u8, imm);

                    memory.sw(128, inst as i32);
                    hart.pc = 128;

                    hart.regs[dest as usize] = 2;
                    hart.regs[src as usize] = reg;

                    hart.execute(&mut memory);

                    assert_eq!(reg, hart.regs[src as usize]);
                    assert_eq!(
                        hart.regs[dest as usize],
                        if (reg as u32) < imm as u32 { 1 } else { 0 }
                    );
                }
            }
        }
    }

    #[test]
    fn add_reg_imm() {
        let mut memory = Memory::new();
        let mut hart = Hart::new();

        for src in 1..32 {
            for dest in (1..32).filter(|v| *v != src) {
                let inst = encode_i(OpCode::REG_IMM as u8, dest, src, AluOps::ADD as u8, 100);
                memory.sw(128, inst as i32);
                hart.pc = 128;

                hart.regs[dest as usize] = 0;
                hart.regs[src as usize] = 50;

                hart.execute(&mut memory);

                assert_eq!(hart.regs[dest as usize], 150);
                assert_eq!(hart.regs[src as usize], 50);
                assert_eq!(hart.pc, 132);
            }
        }
    }

    #[test]
    fn add_reg_reg() {
        let mut memory = Memory::new();
        let mut hart = Hart::new();

        for rs1 in 1..32 {
            for rs2 in rs1 + 1..32 {
                for dest in (1..32).filter(|v| *v != rs1 && *v != rs2) {
                    let inst =
                        encode_r(OpCode::REG_REG as u8, dest, AluOps::ADD as u8, rs1, rs2, 0);
                    memory.sw(128, inst as i32);
                    hart.pc = 128;

                    hart.regs[dest as usize] = 0;
                    hart.regs[rs1 as usize] = 50;
                    hart.regs[rs2 as usize] = 100;

                    hart.execute(&mut memory);

                    assert_eq!(hart.regs[dest as usize], 150);
                    assert_eq!(hart.regs[rs1 as usize], 50);
                    assert_eq!(hart.regs[rs2 as usize], 100);
                    assert_eq!(hart.pc, 132);
                }
            }
        }
    }

    #[test]
    fn xor_reg_reg() {
        let mut memory = Memory::new();
        let mut hart = Hart::new();

        for rs1 in 1..32 {
            for rs2 in (1..32).filter(|v| *v != rs1) {
                for dest in (1..32).filter(|v| *v != rs1 && *v != rs2) {
                    for (lhs, rhs) in [(0, 1), (1, 1), (-1, 1), (i32::MAX, i32::MIN)] {
                        let inst =
                            encode_r(OpCode::REG_REG as u8, dest, AluOps::XOR as u8, rs1, rs2, 0);
                        memory.sw(128, inst as i32);
                        hart.pc = 128;

                        hart.regs[dest as usize] = 0;
                        hart.regs[rs1 as usize] = lhs;
                        hart.regs[rs2 as usize] = rhs;

                        hart.execute(&mut memory);

                        assert_eq!(hart.regs[dest as usize], lhs ^ rhs);
                        assert_eq!(hart.regs[rs1 as usize], lhs);
                        assert_eq!(hart.regs[rs2 as usize], rhs);
                        assert_eq!(hart.pc, 132);
                    }
                }
            }
        }
    }

    // #[test]
    // fn xor_reg_imm() {
    //     let mut memory = Memory::new();
    //     let mut hart = Hart::new();

    //     for rs1 in 1..32 {
    //         for dest in (1..32).filter(|v| *v != rs1) {
    //             for (lhs, rhs) in [(0, 1), (1, 1), (-1, 1), (i32::MAX, i32::MIN)] {
    //                 let inst = encode_i(
    //                     OpCode::REG_IMM as u8,
    //                     dest,
    //                     rs1,
    //                     AluOps::XOR as u8,
    //                     rhs as i16,
    //                 );
    //                 memory.sw(128, inst as i32);
    //                 hart.pc = 128;

    //                 hart.regs[dest as usize] = 0;
    //                 hart.regs[rs1 as usize] = lhs as i32;

    //                 hart.execute(&mut memory);

    //                 assert_eq!(hart.regs[dest as usize], lhs ^ rhs);
    //                 assert_eq!(hart.regs[rs1 as usize], lhs);
    //                 assert_eq!(hart.pc, 132);
    //             }
    //         }
    //     }
    // }
}
