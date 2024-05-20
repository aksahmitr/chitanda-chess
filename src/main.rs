use std::time::Duration;

use board::Board;
use board::PlayerColor;

mod board;
mod lookup;

fn count(ply: u8, board: &mut Board) -> u64 {
    if ply == 5 {
        return 1;
    }
    let mut res: u64 = 0;
    let moves = board.get_moves();
    for pseudo_move in moves {
        let old = board.clone();
        board.make_move(pseudo_move.clone());
        if !board.is_in_check(board.active_color.other().clone()) {
            let cur = count(ply + 1, board);
            res += cur;
            if ply == 0 {
                print!("{:?}{:?}", pseudo_move.origin, pseudo_move.target);
                println!(": {}", cur);
            }
        }
        board.undo_move();
        if board.clone() != old {
            println!("{:#?}", pseudo_move);
            println!("{:#?}\n{:#?}", board.clone(), old);
        }
        assert_eq!(board.clone(), old);
    }
    res
}

fn main() {
    let mut cur =
        Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap();
    //println!("{:#?}", cur.get_moves());
    // let ans = count(0, &mut cur);

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
    //     println!("{}/10", i + 1);
    // }
    // avg /= 10;
    // println!("Average Time taken: {:.2?}", avg);
}
