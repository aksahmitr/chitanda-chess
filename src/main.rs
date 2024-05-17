use board::Board;
use board::PlayerColor;

mod board;
mod lookup;

fn count(ply: u8, board: Board) -> u64 {
    if ply == 3 {
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
                print!("{:?}", pseudo_move);
                println!(" : {}", cur);
            }
        }
    }
    res
}

fn main() {
    let cur =
        Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 0")
            .unwrap();

    // println!("{:#?}", cur);
    // println!(
    //     "{} {}",
    //     cur.can_castle_kingside(PlayerColor::Black),
    //     cur.can_castle_queenside(PlayerColor::Black)
    // );
    // println!("{:#?}", cur.get_moves(PlayerColor::Black));
    // println!("{}", cur.get_moves(PlayerColor::Black).len());
    println!("{}", count(0, cur));
}
