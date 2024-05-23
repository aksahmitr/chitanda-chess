use std::time::Duration;

use board::Board;

mod board;
mod evaluation;
mod lookup;

fn main() {
    let cur =
        Board::from_fen("r1bqkbnr/p1ppp1pp/Bp3p2/2P3B1/3PP3/2N2N2/PP3PPP/R2QK2R b KQkq - 0 8")
            .unwrap();

    //println!("{}", cur.eval());
    //println!("{}", cur.perft(6, 0));
    println!("{:?}", cur.eval_search(5, 0));
    // println!("{:#?}", cur.get_moves());
    // let ans = count(0, cur);

    // println!("{ans}");

    // use std::time::Instant;
    // let mut avg: Duration = Duration::from_micros(0);
    // for i in 0..10 {
    //     let cur = Board::from_fen(
    //         "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10 ",
    //     )
    //     .unwrap();
    //     let now = Instant::now();
    //     let ans = count(0, cur);
    //     let elapsed = now.elapsed();
    //     avg += elapsed;
    //     println!("{}/10 {:.2?}", i + 1, elapsed);
    // }
    // avg /= 10;
    // println!("Average Time taken: {:.2?}", avg);
}
