use hart::Hart;
use memory::Memory;

mod hart;
mod memory;
mod decode;

fn main() {


    let mut memory = Memory::new();
    let mut hart = Hart::new();


    loop {
        hart.execute(&mut memory);
    }
}
