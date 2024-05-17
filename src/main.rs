use board::Board;
use board::PlayerColor;

mod board;
mod lookup;

fn count(ply: u8, board: Board) -> u64 {
    if ply == 6 {
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
            if ply == 0 {
                print!("{:?}{:?}", pseudo_move.origin, pseudo_move.target);
                println!(": {}", cur);
            }
        }
    }
    res
}

fn main() {
    let cur = Board::from_fen(
        "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10 ",
    )
    .unwrap();

    //println!("{}", cur.is_in_check(PlayerColor::Black));

    // println!("{:#?}", cur);
    // println!(
    //     "{} {}",
    //     cur.can_castle_kingside(PlayerColor::Black),
    //     cur.can_castle_queenside(PlayerColor::Black)
    // );
    //println!("{:#?}", cur.get_moves(PlayerColor::Black));
    // println!("{}", cur.get_moves(PlayerColor::Black).len());
    println!("{}", count(0, cur));
}
