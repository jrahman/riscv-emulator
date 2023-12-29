#[macro_export]
macro_rules! asm {
    ($($inst:ident!($($arg:tt),*)),*) => {{
        let mut insts: Vec<u8> = vec![];

        $(insts.extend_from_slice(&$inst!($($arg),*)););*;
        insts
    }};
}

#[repr(u8)]
pub enum OpCode {
    LOAD = 3,
    REG_IMM = 19,
    AUIPC = 23,
    STORE = 35,
    REG_REG = 51,
    LUI = 55,
    BRANCH = 99,
    JALR = 103,
    JAL = 111,
}

impl From<u8> for OpCode {
    fn from(value: u8) -> Self {
        match value {
            3 => Self::LOAD,
            19 => Self::REG_IMM,
            23 => Self::AUIPC,
            35 => Self::STORE,
            51 => Self::REG_REG,
            55 => Self::LUI,
            99 => Self::BRANCH,
            103 => Self::JALR,
            111 => Self::JAL,
            _ => panic!(),
        }
    }
}

#[repr(u8)]
pub enum AluOps {
    ADD = 0b000,
    SLT = 0b010,
    SLTU = 0b011,
    XOR = 0b100,
    OR = 0b110,
    AND = 0b111,
    SLL = 0b001,
    SR = 0b101,

}

macro_rules! alu {
    ($rd:ident, $rs1:ident, $rs2:ident, $op:expr) => {
        $crate::decode::encode_r(
            $crate::assembly::assembler::OpCode::REG_REG as u8,
            $crate::register!($rd),
            $op as u8,
            $crate::register!($rs1),
            $crate::register!($rs2),
            0,
        )
        .to_le_bytes()
    };
    ($rd:ident, $rs1:ident, $imm:expr, $op:expr) => {
        $crate::decode::encode_i(
            $crate::assembly::assembler::OpCode::REG_IMM as u8,
            $crate::register!($rd),
            $crate::register!($rs1),
            $op as u8,
            $imm,
        )
        .to_le_bytes()
    };
}

macro_rules! sw {
    ($rs1:ident, $rs2:ident, $offset:expr) => {
        $crate::decode::encode_s(
            $crate::assembly::assembler::OpCode::STORE as u8,
            $crate::register!($rs1),
            $crate::register!($rs2),
            0b010,
            $offset,
        )
        .to_le_bytes()
    };
}

macro_rules! lw {
    ($rd:ident, $rs:ident, $offset:expr) => {
        $crate::decode::encode_i(
            $crate::assembly::assembler::OpCode::LOAD as u8,
            $crate::register!($rd),
            $crate::register!($rs),
            0b010,
            $offset,
        ).to_le_bytes()
    };
}

macro_rules! li {
    ($rd:ident, $imm:expr) => {{
        let mut inst = u32::from_le_bytes(lui!($rd, $imm)) as u64;
        inst |= (u32::from_le_bytes(add!($rd, x0, $imm)) as u64) << 32;
        inst.to_le_bytes()
    }}
}

macro_rules! add {
    ($($args:tt)*) => {
        alu!($($args)*, $crate::assembly::assembler::AluOps::ADD)
    };
}

macro_rules! sra {
    ($rd:ident, $rs1:ident, $rs2:ident) => {
        $crate::decode::encode_r(
            $crate::assembly::assembler::OpCode::REG_REG as u8,
            $crate::register!($rd),
            $crate::assembly::assembler::AluOps::SR as u8,
            $crate::register!($rs1),
            $crate::register!($rs2),
            (1u8 << 7),
        )
        .to_le_bytes()
    };
    ($rd:ident, $rs1:ident, $imm:expr) => {
        $crate::decode::encode_i(
            $crate::assembly::assembler::OpCode::REG_IMM as u8,
            $crate::register!($rd),
            $crate::register!($rs1),
            AluOps::SR,
            ($imm & 0b1111) | (1 << 11),
        )
        .to_le_bytes()
    };
}

macro_rules! sub {
    ($rd:ident, $rs1:ident, $rs2:ident) => {
        $crate::decode::encode_r(
            $crate::assembly::assembler::OpCode::REG_REG as u8,
            $crate::register!($rd),
            // SUB and ADD are same opcode but with different funct7 value
            $crate::assembly::assembler::AluOps::ADD as u8,
            $crate::register!($rs1),
            $crate::register!($rs2),
            0b0100000,
        )
        .to_le_bytes()
    };
}

macro_rules! xor {
    ($($args:tt)*) => {
        alu!($($args)*, $crate::assembly::assembler::AluOps::XOR)
    };
}

macro_rules! or {
    ($($args:tt)*) => {
        alu!($($args)*, $crate::assembly::assembler::AluOps::OR)
    };
}

macro_rules! and {
    ($($args:tt)*) => {
        alu!($($args)*, $crate::assembly::assembler::AluOps::AND)
    };
}

macro_rules! slt {
    ($($args:tt)*) => {
        alu!($($args)*, $crate::assembly::assembler::AluOps::SLT)
    };
}

macro_rules! sltu {
    ($($args:tt)*) => {
        alu!($($args)*, $crate::assembly::assembler::AluOps::SLTU)
    };
}

macro_rules! jal {
    ($rd:ident, $offset:expr) => {
        $crate::decode::encode_j(
            $crate::assembly::assembler::OpCode::JAL as u8,
            $crate::register!($rd),
            $offset,
        )
        .to_le_bytes()
    };
    ($offset:expr) => {
        $crate::decode::encode_j(
            $crate::assembly::assembler::OpCode::JAL as u8,
            register!(x1),
            $offset,
        )
        .to_le_bytes()
    };
}

macro_rules! jalr {
    ($rd:ident, $rs1:ident, $offset:expr) => {
        $crate::decode::encode_i(
            $crate::assembly::assembler::OpCode::JALR as u8,
            $crate::register!($rd),
            $crate::register!($rs1),
            0,
            $offset,
        )
        .to_le_bytes()
    };
    ($rs1:ident) => {
        $crate::decode::encode_i(
            $crate::assembly::assembler::OpCode::JALR as u8,
            $crate::register!(x1),
            $crate::register!($rs1),
            0,
            0,
        )
        .to_le_bytes()
    };
}

macro_rules! beq {
    ($rs1:ident, $rs2:ident, $offset:expr) => {
        $crate::decode::encode_b(
            $crate::assembly::assembler::OpCode::BRANCH as u8,
            $crate::register!($rs1),
            $crate::register!($rs2),
            0b000,
            $offset,
        )
        .to_le_bytes()
    };
}

macro_rules! bne {
    ($rs1:ident, $rs2:ident, $offset:expr) => {
        $crate::decode::encode_b(
            $crate::assembly::assembler::OpCode::BRANCH as u8,
            $crate::register!($rs1),
            $crate::register!($rs2),
            0b001,
            $offset,
        )
        .to_le_bytes()
    };
}

macro_rules! blt {
    ($rs1:ident, $rs2:ident, $offset:expr) => {
        $crate::decode::encode_b(
            $crate::assembly::assembler::OpCode::BRANCH as u8,
            $crate::register!($rs1),
            $crate::register!($rs2),
            0b100,
            $offset,
        )
        .to_le_bytes()
    };
}

macro_rules! bge {
    ($rs1:ident, $rs2:ident, $offset:expr) => {
        $crate::decode::encode_b(
            $crate::assembly::assembler::OpCode::BRANCH as u8,
            $crate::register!($rs1),
            $crate::register!($rs2),
            0b101,
            $offset,
        )
        .to_le_bytes()
    };
}

macro_rules! bltu {
    ($rs1:ident, $rs2:ident, $offset:expr) => {
        $crate::decode::encode_b(
            $crate::assembly::assembler::OpCode::BRANCH as u8,
            $crate::register!($rs1),
            $crate::register!($rs2),
            0b110,
            $offset,
        )
        .to_le_bytes()
    };
}

macro_rules! bgeu {
    ($rs1:ident, $rs2:ident, $offset:expr) => {
        $crate::decode::encode_b(
            $crate::assembly::assembler::OpCode::BRANCH as u8,
            $crate::register!($rs1),
            $crate::register!($rs2),
            0b111,
            $offset,
        )
        .to_le_bytes()
    };
}

macro_rules! beqz {
    ($rs:ident, $offset:expr) => {
        beq!($rs, x0, $offset)
    };
}

macro_rules! bnez {
    ($rs:ident, $offset:expr) => {
        beq!($rs, x0, $offset)
    };
}

macro_rules! blez {
    ($rs:ident, $offset:expr) => {
        bge!(x0, $rs, $offset)
    };
}

macro_rules! bgez {
    ($rs:ident, $offset:expr) => {
        bge!($rs, x0, $offset)
    };
}

macro_rules! bltz {
    ($rs:ident, $offset:expr) => {
        blt!($rs, x0, $offset)
    };
}

macro_rules! bgtz {
    ($rs:ident, $offset:expr) => {
        blt!(x0, $rs, $offset)
    };
}

macro_rules! bgt {
    ($rs:ident, $rt:ident, $offset:expr) => {
        blt!($rt, $rs, $offset)
    };
}

macro_rules! ble {
    ($rs:ident, $rt:ident, $offset:expr) => {
        bge!($rt, $rs, $offset)
    };
}

macro_rules! bgtu {
    ($rs:ident, $rt:ident, $offset:expr) => {
        bltu!($rt, $rs, $offset)
    };
}

macro_rules! bleu {
    ($rs:ident, $rt:ident, $offset:expr) => {
        bgeu!($rt, $rs, $offset)
    };
}

macro_rules! seqz {
    ($rd:ident, $rs:ident) => {
        sltu!($rd, $rs, 1)
    };
}

macro_rules! sneqz {
    ($rd:ident, $rs:ident) => {
        sltu!($rd, x0, $rs)
    };
}

macro_rules! sltz {
    ($rd:ident, $rs:ident) => {
        slt!($rd, $rs, x0)
    };
}

macro_rules! sgtz {
    ($rd:ident, $rs:ident) => {
        slt!($rd, x0, $rs)
    };
}

macro_rules! mv {
    ($rd:ident, $rs1:ident) => {
        add!($rd, $rs1, 0)
    };
}

macro_rules! nop {
    () => {
        add!(x0, x0, 0)
    };
}

macro_rules! not {
    ($rd:ident, $rs:ident) => {
        xor!($rd, $rs, -1)
    };
}

macro_rules! neg {
    ($rd:ident, $rs:ident) => {
        sub!($rd, x0, $rs)
    };
}

macro_rules! auipc {
    ($rd:ident, $offset:expr) => {
        $crate::decode::encode_u(
            $crate::assembly::assembler::OpCode::AUIPC as u8,
            $crate::register!($rd),
            $offset,
        )
        .to_le_bytes()
    };
}

macro_rules! lui {
    ($rd:ident, $imm:expr) => {
        $crate::decode::encode_u(
            $crate::assembly::assembler::OpCode::LUI as u8,
            $crate::register!($rd),
            ($imm) as u32 as i32,
        )
        .to_le_bytes()
    }
}

macro_rules! j {
    ($offset:expr) => {
        jal!(x0, $offset)
    };
}

macro_rules! jr {
    ($rs1:ident) => {
        jalr!(x0, $rs1, 0)
    };
}

macro_rules! ret {
    () => {
        jalr!(x0, x1, 0)
    };
}

macro_rules! call {
    ($offset:expr) => {{
        let mut inst = u32::from_le_bytes(auipc!(
            x1,
            (($offset) as u32 >> 20) as i32 + (($offset) as i32) & (1 << 11)
        )) as u64;
        inst |= (u32::from_le_bytes(jalr!(x1, x1, ($offset) & 0b111111111111)) as u64) << 32;
        inst.to_le_bytes()
    }};
}

macro_rules! tail {
    ($offset:expr) => {{
        let mut inst = u32::from_le_bytes(auipc!(
            x6,
            (($offset) as u32 >> 20) as i32 + (($offset) as i32) & (1 << 11)
        )) as u64;
        inst |= (u32::from_le_bytes(jalr!(x0, x6, ($offset) & 0b111111111111)) as u64) << 32;
        inst.to_le_bytes()
    }};
}

#[cfg(test)]
mod test {

    /// Validate the macros expand as required in a simple test case, check for
    /// both Reg-Imm and Reg-Reg forms of 3 Reg instructions
    #[test]
    fn generate() {
        asm! {
            add!(x1, x2, x5),
        add!(x2, x0, 15),

        xor!(x6, x28, x4),
        xor!(x9, x25, 19),

        or!(x19, x31, x2),
        or!(x12, x18, 9),

        and!(x8, x25, x12),
        and!(x1, x29, 5),

        jal!(x5, 1234),

        beq!(x0, x1, 12),
        bne!(x6, x0, 4),
        blt!(x0, x2, 16),
        bge!(x1, x2, 32),
        bltu!(x7, x1, 64),
        bgeu!(x11, x21, 128),

        beqz!(x1, 12),
        bnez!(x6, 4),
        blez!(x2, 16),
        bgez!(x2, 32),
        bltz!(x1, 64),
        bgtz!(x21, 128),

        bgt!(x0, x1, 12),
        ble!(x6, x0, 4),
        bgtu!(x0, x2, 16),
        bleu!(x1, x2, 32),

        slt!(x1, x0, 2),
        slt!(x1, x22, x20),
        sltu!(x1, x0, 2),
        sltu!(x1, x22, x20),

        sub!(x19, x21, x30),

        seqz!(x1, x2),
        sneqz!(x2, x17),
        sltz!(x29, x4),
        sgtz!(x23, x11),

        mv!(x5, x3),

        nop!(),

        not!(x5, x12),
        neg!(x1, x27),

        j!(1028),

        jr!(x5),

        call!(16),

        ret!(),

        tail!(124)
        };
    }
}
