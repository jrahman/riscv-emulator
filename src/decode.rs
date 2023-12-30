/// Basic instruction decoding handlers for the initial set of RVA32I
/// instructions. Decoders are implemented for instruction types:
/// R, S, U, I, B, and J
///

/// Instruction encoding for Reg-Reg instructions, with a pair of source
/// registers and a destination register, along with 10 bits of sub-operation
/// type values:
///
///  31      25 24    20 19    15 14    12 11     7 6      0
/// |----------|--------|--------|--------|--------|--------|
/// |  funct7  |  src2  |  src1  | funct3 |  dest  | opcode |   
/// |----------|--------|--------|--------|--------|--------|
pub fn decode_r(inst: u32) -> (u8, u8, u8, u8, u8, u8) {
    let opcode = inst & 127;
    let rd = (inst >> 7) & 31;
    let rs1 = (inst >> 15) & 31;
    let rs2 = (inst >> 20) & 31;
    let funct3 = (inst >> 12) & 7;
    let funct7 = (inst >> 25) & 127;
    (
        opcode as u8,
        rd as u8,
        rs1 as u8,
        rs2 as u8,
        funct3 as u8,
        funct7 as u8,
    )
}

pub fn encode_r(opcode: u8, dest: u8, funct3: u8, src1: u8, src2: u8, funct7: u8) -> u32 {
    (opcode as u32 & 127)
        | ((dest as u32 & 31) << 7)
        | ((src1 as u32 & 31) << 15)
        | ((src2 as u32 & 31) << 20)
        | ((funct3 as u32 & 7) << 12)
        | ((funct7 as u32 & 127) << 25)
}

/// Instruction encoding for Store instructions, using a base address
/// register, and immediate offset, and a source register to be written
/// to memory:
///
///  31       25 24    20 19    15 14    12 11       7 6      0
/// |-----------|--------|--------|--------|----------|--------|
/// | imm[11:5] |  src2  |  src1  | funct3 | imm[4:0] | opcode |
/// |-----------|--------|--------|--------|----------|--------|
pub fn decode_s(inst: u32) -> (u8, u8, u8, u8, i16) {
    let opcode = inst & 127;
    let imm1 = ((inst >> 7) & 31) as i16;
    let funct3 = (inst >> 12) & 7;
    let rs1 = (inst >> 15) & 31;
    let rs2 = (inst >> 20) & 31;
    let imm2 = ((inst >> 25) & 127) as i16;
    (
        opcode as u8,
        funct3 as u8,
        rs1 as u8,
        rs2 as u8,
        imm2 << 5 | imm1,
    )
}

pub fn encode_s(opcode: u8, funct3: u8, rs1: u8, rs2: u8, imm: i16) -> u32 {
    (opcode as u32 & 127)
        | ((imm as u32 & 0b11111) << 7)
        | ((funct3 as u32 & 0b111) << 12)
        | ((rs1 as u32 & 31) << 15)
        | ((rs2 as u32 & 31) << 20)
        | ((imm as u32 & 127) << 25)
}

/// Instruction encoding for a Upper immediate instruction which is used for
/// LUI and AUIPC instructions to build constants. Contains pieces of the
/// immediate along with the destination register:
///
///  31                                      12 11       7 6      0
/// |------------------------------------------|--------|----------|
/// |                 imm[31:12]               |   dst  |  opcode  |
/// |------------------------------------------|--------|----------|
pub fn decode_u(inst: u32) -> (u8, u8, i32) {
    let opcode = inst & 127;
    let rd = (inst >> 7) & 31;
    let imm = inst >> 12;
    (opcode as u8, rd as u8, (imm << 12) as i32)
}

pub fn encode_u(opcode: u8, rd: u8, imm: i32) -> u32 {
    (opcode as u32 & 127) | ((rd as u32 & 31) << 7) | ((imm as u32 & 0b11111111111111111111) << 12)
}

/// Instruction encoding for Reg-Imm instructions that encodes a dest register,
/// a source register, and a 12 bit immediate value (sign-extended) along with
/// function sub-opcode:
///
///  31                   20 19    15 14    12 11     7 6      0
/// |-----------------------|--------|--------|--------|--------|
/// |        imm[11:0]      |  src1  | funct3 |  dest  | opcode |   
/// |-----------------------|--------|--------|--------|--------|
pub fn decode_i(inst: u32) -> (u8, u8, u8, u8, i16) {
    let opcode = inst & 127;
    let rd: u32 = (inst >> 7) & 31;
    let rs1 = (inst >> 15) & 31;
    let funct3 = (inst >> 12) & 7;
    let imm = inst >> 20;
    let imm = ((imm as i32) << 20) >> 20;
    (opcode as u8, rd as u8, rs1 as u8, funct3 as u8, imm as i16)
}

pub fn encode_i(opcode: u8, dest: u8, src: u8, funct3: u8, imm: i16) -> u32 {
    (opcode as u32 & 127)
        | ((dest as u32) << 7)
        | ((src as u32 & 31) << 15)
        | ((funct3 as u32 & 7) << 12)
        | ((imm as u32) << 20)
}

/// Instruction encoding for Branch instructions that encodes a src register,
/// a target base register, and 12 bit immediate value (sign-extended) offset
/// which is added to the target base register, and finally a funct3 values
/// that determines how the source register is used for conditional branches:
///
///  31      30       25 24    20 19    15 14  12 11       8 7       6      0    
/// |-------|-----------|--------|--------|------|----------|-------|--------|
/// |imm[12]| imm[10:5] |  src2  |  src1  | fun3 | imm[4:1] |imm[11]| opcode |
/// |-------|-----------|--------|--------|------|----------|-------|--------|
pub fn decode_b(inst: u32) -> (u8, u8, u8, u8, i16) {
    let opcode = inst & 127;
    let rs1 = (inst >> 15) & 31;
    let rs2 = (inst >> 20) & 31;
    let imm1 = ((inst >> 7) & 1) as i32;
    let imm2 = ((inst >> 8) & 15) as i32;
    let imm3 = ((inst >> 25) & 63) as i32;
    let imm4 = (inst as i32 >> 31) as i32;
    let funct3 = (inst >> 12) & 7;
    (
        opcode as u8,
        rs1 as u8,
        rs2 as u8,
        funct3 as u8,
        (imm2 << 1 | imm3 << 5 | imm1 << 11 | imm4 << 12) as i16,
    )
}

pub fn encode_b(opcode: u8, src1: u8, src2: u8, funct3: u8, imm: i16) -> u32 {
    (opcode as u32 & 127)
        | (((imm as u32 >> 11) & 1) << 7)
        | (((imm as u32 >> 1) & 15) << 8)
        | ((funct3 as u32 & 7) << 12)
        | ((src1 as u32 & 31) << 15)
        | ((src2 as u32 & 31) << 20)
        | (((imm as u32 >> 5) & 63) << 25)
        | (((imm as u32 >> 12) & 1) << 31)
}

/// Instruction encoding for Jump instructions, encodes a 20 bit immediate
/// offset from the current PC, a destination register to save the current
/// Program Counter register:
///
///  31        30             21 20        19        12 11     7 6        0
/// |---------|-----------------|---------|------------|--------|----------|
/// | imm[20] |    imm[10:1]    | imm[11] | imm[19:12] |  dest  |  opcode  |
/// |---------|-----------------|---------|------------|--------|----------|
pub fn decode_j(inst: u32) -> (u8, u8, i32) {
    let opcode = inst & 127;
    let rd = (inst >> 7) & 31;
    let imm1 = ((inst >> 12) & 255) as i32;
    let imm2 = ((inst >> 20) & 1) as i32;
    let imm3 = ((inst >> 21) & 1023) as i32;
    let imm4 = (inst as i32) >> 31;
    (
        opcode as u8,
        rd as u8,
        imm3 << 1 | imm2 << 11 | imm1 << 12 | imm4 << 20,
    )
}

pub fn encode_j(opcode: u8, dest: u8, imm: i32) -> u32 {
    (opcode as u32 & 127)
        | ((dest as u32 & 31) << 7)
        | (((imm as u32 >> 12) & 255) << 12)
        | (((imm as u32 >> 11) & 1) << 20)
        | (((imm as u32 >> 1) & 1023) << 21)
        | (((imm as u32 >> 20) & 1) << 31)
}
