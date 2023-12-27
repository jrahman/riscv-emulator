/// Represents the memory state of the machine in question, including
/// potentially shared memory in the future. Start with a simple 64kb
/// address space with no-MMU
pub struct Memory {
    physical_memory: [u8; 64 * 1024],
}

impl Memory {
    pub fn new() -> Self {
        Self {
            physical_memory: [0; 64 * 1024],
        }
    }

    pub fn lw(&self, addr: u32) -> i32 {
        let addr = addr as usize;
        i32::from_le_bytes(self.physical_memory[addr..addr + 4].try_into().unwrap())
    }

    pub fn lh(&self, addr: u32) -> i32 {
        let addr = addr as usize;
        let hword = u16::from_le_bytes(self.physical_memory[addr..addr + 2].try_into().unwrap());
        hword as i32
    }

    pub fn lhu(&self, addr: u32) -> i32 {
        let addr = addr as usize;
        let hword = u16::from_le_bytes(self.physical_memory[addr..addr + 2].try_into().unwrap());
        unsafe { std::mem::transmute(hword as u32) }
    }

    pub fn lb(&self, addr: u32) -> i32 {
        let addr = addr as usize;
        let bword = self.physical_memory[addr];
        bword as i32
    }

    pub fn lbu(&self, addr: u32) -> i32 {
        let addr = addr as usize;
        let bword = self.physical_memory[addr];
        unsafe { std::mem::transmute(bword as u32) }
    }

    pub fn sw(&mut self, addr: u32, value: i32) {
        let addr = addr as usize;
        self.physical_memory[addr..addr + 4].copy_from_slice(&value.to_le_bytes());
    }

    pub fn sh(&mut self, addr: u32, value: i16) {
        let addr = addr as usize;
        self.physical_memory[addr..addr + 2].copy_from_slice(&value.to_le_bytes());
    }

    pub fn sb(&mut self, addr: u32, value: i8) {
        let addr = addr as usize;
        self.physical_memory[addr..addr + 1].copy_from_slice(&value.to_le_bytes());
    }

    pub fn load_inst(&self, addr: u32) -> u32 {
        let addr = addr as usize;
        u32::from_le_bytes(self.physical_memory[addr..addr + 4].try_into().unwrap())
    }
}
