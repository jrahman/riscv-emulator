use std::time::Instant;

use riscv::{hart::Hart, memory::Memory, *};

use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("alu", |b| {
        b.iter_custom(|iters| {
            let mut memory = Memory::new();
            let mut hart = Hart::new();

            let iters = (std::cmp::max(5, iters) / 5) as u32;

            let buffer = black_box(asm! {
                li!(a3, iters),
                lw!(a4, zero, 52),
                li!(a5, 0),
                li!(a0, 1),
                add!(a4, a4, a5),
                add!(a0, a0, a5),
                sra!(a0, a0, a5),
                add!(a5, a5, 1),
                bne!(a3 , a5 , (-16)),
                sw!(zero, a4, (-48)),
                ebreak!(),
                li!(a0, 1),
                ebreak!()
            });

            memory.load_program(64, &buffer);
            hart.set_pc(64);

            let start = Instant::now();
            while hart.is_running() {
                black_box(&memory);
                black_box(&hart);
                hart.execute(&mut memory);
            }
            let duration = start.elapsed();

            assert_eq!(iters, hart.get_reg(register!(a3)) as u32);

            duration
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
