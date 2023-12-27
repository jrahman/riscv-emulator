macro_rules! asm {
    ($($inst:ident!($($arg:tt),*)),*) => {{
        let mut insts: Vec<u8> = vec![];

        $(insts.extend_from_slice(&u32::to_le_bytes($inst!($($arg),*))););*;
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

macro_rules! register {
    (x0) => {
        0
    };
    (x1) => {
        1
    };
    (x2) => {
        2
    };
    (x3) => {
        3
    };
    (x4) => {
        4
    };
    (x5) => {
        5
    };
    (x6) => {
        6
    };
    (x7) => {
        7
    };
    (x8) => {
        8
    };
    (x9) => {
        9
    };
    (x10) => {
        10
    };
    (x11) => {
        11
    };
    (x12) => {
        12
    };
    (x13) => {
        13
    };
    (x14) => {
        14
    };
    (x15) => {
        15
    };
    (x16) => {
        16
    };
    (x17) => {
        17
    };
    (x18) => {
        18
    };
    (x19) => {
        19
    };
    (x20) => {
        20
    };
    (x21) => {
        21
    };
    (x22) => {
        22
    };
    (x23) => {
        23
    };
    (x24) => {
        24
    };
    (x25) => {
        25
    };
    (x26) => {
        26
    };
    (x27) => {
        27
    };
    (x28) => {
        28
    };
    (x29) => {
        29
    };
    (x30) => {
        30
    };
    (x31) => {
        31
    };
}

macro_rules! alu {
    ($dest:ident, $src1:ident, $src2:ident, $op:expr) => {
        $crate::decode::encode_r(
            $crate::assembler::OpCode::REG_REG as u8,
            register!($dest),
            $op as u8,
            register!($src1),
            register!($src2),
            0,
        )
    };
    ($dest:ident, $src1:ident, $imm:literal, $op:expr) => {
        $crate::decode::encode_i(
            $crate::assembler::OpCode::REG_IMM as u8,
            register!($dest),
            register!($src1),
            $op as u8,
            $imm,
        )
    };
}

macro_rules! add {
    ($($args:tt)*) => {
        alu!($($args)*, $crate::assembler::AluOps::ADD)
    };
}

macro_rules! sub {
    ($dest:ident, $src1:ident, $src2:ident) => {
        $crate::decode::encode_r(
            $crate::assembler::OpCode::REG_REG as u8,
            register!($dest),
            // SUB and ADD are same opcode but with different funct7 value
            $crate::assembler::AluOps::ADD as u8,
            register!($src1),
            register!($src2),
            0b0100000,
        )
    };
}

macro_rules! xor {
    ($($args:tt)*) => {
        alu!($($args)*, $crate::assembler::AluOps::XOR)
    };
}

macro_rules! jal {
    ($rd:ident, $offset:literal) => {
        $crate::decode::encode_j(
            $crate::assembler::OpCode::JAL as u8,
            register!($rd),
            $offset,
        )
    };
    ($offset:literal) => {
        $crate::decode::encode_j($crate::assembler::OpCode::JAL as u8, register!(x1), $offset)
    };
}

macro_rules! jalr {
    ($rd:ident, $rs1:ident, $offset:literal) => {
        $crate::decode::encode_i(
            $crate::assembler::OpCode::JALR as u8,
            register!($rd),
            register!($rs1),
            0,
            $offset,
        )
    };
    ($rs1:ident) => {
        $crate::decode::encode_i(
            $crate::assembler::OpCode::JALR as u8,
            register!(x1),
            register!($rs1),
            0,
            0,
        )
    };
}

macro_rules! beq {
    ($rs1:ident, $rs2:ident, $offset:literal) => {
        $crate::decode::encode_b(
            $crate::assembler::OpCode::BRANCH as u8,
            register!($rs1),
            register!($rs2),
            0b000,
            $offset,
        )
    };
}

macro_rules! mv {
    ($dest:ident, $src1:ident) => {
        add!($dest, $src1, 0)
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

macro_rules! j {
    ($offset:literal) => {
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

#[cfg(test)]
mod test {

    /// Validate the macros expand as required in a simple test case, check for
    /// both Reg-Imm and Reg-Reg forms of 3 Reg instructions
    #[test]
    fn generate() {
        add!(x1, x2, x5);
        add!(x2, x0, 15);

        xor!(x6, x28, x4);
        xor!(x9, x25, 19);

        jal!(x5, 1234);

        beq!(x0, x1, -12);

        sub!(x19, x21, x30);

        mv!(x5, x3);

        nop!();

        not!(x5, x12);

        neg!(x1, x27);

        j!(1028);

        jr!(x5);

        ret!();

        asm!(add!(x1, x4, 3), nop!(), nop!(), not!(x1, x1));
    }
}
