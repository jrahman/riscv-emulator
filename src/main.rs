use hart::Hart;
use memory::Memory;

mod decode;
mod hart;
mod memory;

fn main() {
    let mut memory = Memory::new();
    let mut hart = Hart::new();

    loop {
        hart.execute(&mut memory);
    }
}
