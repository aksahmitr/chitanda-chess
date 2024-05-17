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
        if ply == 0 {
            print!("{:?}", pseudo_move.clone());
        }
        next.make_move(pseudo_move);
        if !next.is_in_check(color) {
            let cur = count(ply + 1, next);
            res += cur;
            if ply == 0 {
                println!(" : {}", cur);
            }
        }
    }
    res
}

fn main() {
    let cur = Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap();
    println!("{}", count(0, cur));
    //println!("{}", cur.get_moves(PlayerColor::Black).len());
}
