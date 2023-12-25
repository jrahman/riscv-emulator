/// Basic instruction decoding handlers for the initial set of RVA32I
/// instructions. Decoders are implemented for instruction types:
/// R, S, U, I, B, and J
///

fn decode_R(inst: u32) -> (u8, u8, u8, u8, u8, u8) {
    let opcode = inst & 63;
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

fn decode_S(inst: u32) -> (u8, u8, u8, u16) {
    let opcode = inst & 63;
    let imm1 = (inst >> 7) & 31;
    let rs1 = (inst >> 15) & 31;
    let rs2 = (inst >> 20) & 31;
    let imm2 = (inst >> 25) & 127;
    (
        opcode as u8,
        rs1 as u8,
        rs2 as u8,
        (imm2 as u16) << 5 | (imm1 as u16),
    )
}

fn decode_U(inst: u32) -> (u8, u8, u32) {
    let opcode = inst & 63;
    let rd = (inst >> 7) & 31;
    let imm = (inst >> 12);
    (opcode as u8, rd as u8, imm as u32)
}

fn decode_I(inst: u32) -> (u8, u8, u8, u8, u16) {
    let opcode = inst & 63;
    let rd: u32 = (inst >> 7) & 31;
    let rs1 = (inst >> 15) & 31;
    let funct3 = (inst >> 12) & 7;
    let imm = inst >> 25;
    (opcode as u8, rd as u8, rs1 as u8, funct3 as u8, imm as u16)
}

fn decode_B(inst: u32) -> (u8, u8, u8, u8, u32) {
    let opcode = inst & 63;
    let rs1 = (inst >> 15) & 31;
    let rs2 = (inst >> 20) & 31;
    let imm1 = (inst >> 7) & 1;
    let imm2 = (inst >> 8) & 15;
    let imm3 = (inst >> 25) & 63;
    let imm4 = (inst >> 31) & 1;
    let funct3 = (inst >> 12) & 7;
    (
        opcode as u8,
        rs1 as u8,
        rs2 as u8,
        funct3 as u8,
        imm2 << 1 | imm3 << 5 | imm1 << 11 | imm4 << 12,
    )
}

fn decode_J(inst: u32) -> (u8, u8, u32) {
    let opcode = inst & 63;
    let rd = (inst >> 7) & 31;
    let imm1 = (inst >> 12) & 511;
    let imm2 = (inst >> 20) & 1;
    let imm3 = (inst >> 21) & 1023;
    let imm4 = (inst >> 31);
    (
        opcode as u8,
        rd as u8,
        imm3 << 1 | imm2 << 11 | imm1 << 12 | imm4 << 20,
    )
}
