mod board;
mod lookup;

fn main() {
    let cur =
        board::Board::from_fen("rnbqkbnr/ppp1p1pp/3p4/4Pp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3")
            .unwrap();

    println!("{:#?}", cur.get_moves(board::PlayerColor::White));
}
