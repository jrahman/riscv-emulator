/// Basic instruction decoding handlers for the initial set of RVA32I
/// instructions. Decoders are implemented for instruction types:
/// R, S, U, I, B, and J
///

pub fn decode_R(inst: u32) -> (u8, u8, u8, u8, u8, u8) {
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

pub fn decode_S(inst: u32) -> (u8, u8, u8, u8, i16) {
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

pub fn decode_U(inst: u32) -> (u8, u8, i32) {
    let opcode = inst & 127;
    let rd = (inst >> 7) & 31;
    let imm = (inst >> 12);
    (opcode as u8, rd as u8, (imm << 12) as i32)
}

pub fn decode_I(inst: u32) -> (u8, u8, u8, u8, i16) {
    let opcode = inst & 127;
    let rd: u32 = (inst >> 7) & 31;
    let rs1 = (inst >> 15) & 31;
    let funct3 = (inst >> 12) & 7;
    let imm = inst >> 25;
    (opcode as u8, rd as u8, rs1 as u8, funct3 as u8, imm as i16)
}

pub fn decode_B(inst: u32) -> (u8, u8, u8, u8, i16) {
    let opcode = inst & 127;
    let rs1 = (inst >> 15) & 31;
    let rs2 = (inst >> 20) & 31;
    let imm1 = ((inst >> 7) & 1) as i16;
    let imm2 = ((inst >> 8) & 15) as i16;
    let imm3 = ((inst >> 25) & 63) as i16;
    let imm4 = ((inst >> 31) & 1) as i16;
    let funct3 = (inst >> 12) & 7;
    (
        opcode as u8,
        rs1 as u8,
        rs2 as u8,
        funct3 as u8,
        imm2 << 1 | imm3 << 5 | imm1 << 11 | imm4 << 12,
    )
}

pub fn decode_J(inst: u32) -> (u8, u8, i32) {
    let opcode = inst & 127;
    let rd = (inst >> 7) & 31;
    let imm1 = ((inst >> 12) & 511) as i32;
    let imm2 = ((inst >> 20) & 1) as i32;
    let imm3 = ((inst >> 21) & 1023) as i32;
    let imm4 = (inst >> 31) as i32;
    (
        opcode as u8,
        rd as u8,
        imm3 << 1 | imm2 << 11 | imm1 << 12 | imm4 << 20,
    )
}
