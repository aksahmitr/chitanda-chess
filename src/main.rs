use std::time::Duration;

use board::Board;

mod board;
mod evaluation;
mod lookup;

fn main() {
    use std::time::Instant;
    let mut avg: Duration = Duration::from_micros(0);
    for i in 0..10 {
        let cur =
            Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap();
        let now = Instant::now();

        let ans = cur.alpha_beta(9, i32::min_value(), i32::max_value());

        let elapsed = now.elapsed();
        avg += elapsed;
        println!("{}/10 {:.2?}", i + 1, elapsed);
    }
    avg /= 10;
    println!("Average Time taken: {:.2?}", avg);
}
