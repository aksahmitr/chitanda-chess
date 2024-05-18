use std::time::Duration;

use board::Board;
use board::PlayerColor;

mod board;
mod lookup;

fn count(ply: u8, board: Board) -> u64 {
    if ply == 5 {
        return 1;
    }
    let mut res: u64 = 0;
    let color = if ply % 2 == 0 {
        PlayerColor::White
    } else {
        PlayerColor::Black
    };
    let moves = board.get_moves(color);
    for pseudo_move in moves {
        let mut next = board.clone();
        next.make_move(pseudo_move.clone());
        if !next.is_in_check(color) {
            let cur = count(ply + 1, next);
            res += cur;
            // if ply == 0 {
            //     print!("{:?}{:?}", pseudo_move.origin, pseudo_move.target);
            //     println!(": {}", cur);
            // }
        }
    }
    res
}

fn main() {
    //r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10
    let cur =
        Board::from_fen("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10")
            .unwrap();

    // println!("{:#?}", cur.get_moves(PlayerColor::White));

    // let ans = count(0, cur.clone());

    // println!("{ans}");

    use std::time::Instant;
    let mut avg: Duration = Duration::from_micros(0);
    for i in 0..100 {
        let now = Instant::now();
        let ans = count(0, cur.clone());
        let elapsed = now.elapsed();
        avg += elapsed;
        println!("{}/100 {:.2?}", i + 1, elapsed);
    }
    avg /= 100;
    println!("Average Time taken: {:.2?}", avg);
}
