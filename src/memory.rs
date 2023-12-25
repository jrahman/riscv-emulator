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

    pub fn load(&self, addr: u64) -> u64 {
        let addr = addr as usize;
        u64::from_le_bytes(self.physical_memory[addr..addr + 8].try_into().unwrap())
    }

    pub fn load_inst(&self, addr: u64) -> u32 {
        let addr = addr as usize;
        u32::from_le_bytes(self.physical_memory[addr..addr+4].try_into().unwrap())
    }
}
