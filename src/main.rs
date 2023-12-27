use hart::Hart;
use memory::Memory;

#[macro_use]
mod assembler;
mod decode;
mod hart;
mod memory;

fn main() {
    let mut memory = Memory::new();
    let mut hart = Hart::new();

    asm!(beq!(x0, x1, 32), nop!(), jal!(8));

    loop {
        hart.execute(&mut memory);
    }
}
