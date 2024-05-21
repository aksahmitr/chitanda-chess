use std::time::Duration;

use board::Board;

mod board;
mod evaluation;
mod lookup;

fn count(ply: u8, board: Board) -> u64 {
    if ply == 5 {
        return 1;
    }
    let mut res: u64 = 0;
    let moves = board.get_moves();
    for pseudo_move in moves {
        let mut next = board.clone();
        next.make_move(pseudo_move.clone());
        if !next.is_in_check(next.active_color.other().clone()) {
            let cur = count(ply + 1, next);
            res += cur;
            // if ply == 0 {
            //     print!("{:?}{:?}", pseudo_move.origin, pseudo_move.target);
            //     println!(": {}", cur);
            // }
        }
        //board.undo_move();
    }
    res
}

fn main() {
    let cur = Board::from_fen("rnb1kbnr/pppp1ppp/8/4p3/7R/8/PPPPPPP1/RNBQKB2 b Qkq - 0 4").unwrap();

    println!("{}", cur.eval());
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
