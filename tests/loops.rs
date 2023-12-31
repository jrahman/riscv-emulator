use riscv::{hart::Hart, memory::Memory, *};

#[test]
fn basic_loop_with_arithmetic() {
    let mut memory = Memory::new();
    let mut hart = Hart::new();

    let buffer = asm! {
        li!(a0, 10000),
        mv!(a3, a0),
        ble!(a0, zero, 48),
        lw!(a4, zero, 52),
        li!(a5, 0),
        li!(a0, 1),
        add!(a4, a4, a5),
        add!(a0, a0, a5),
        sra!(a0, a0, a5),
        add!(a5, a5, 1),
        bne!(a3, a5, (-16)),
        sw!(zero, a4, (-48)),
        ebreak!(),
        li!(a0, 1),
        ebreak!()
    };

    memory.load_program(64, &buffer);
    hart.set_pc(64);

    while hart.is_running() {
        hart.execute(&mut memory);
    }

    assert_eq!(hart.get_reg(register!(a3)), 10000);
    assert_eq!(hart.get_reg(register!(a5)), 10000);
}
