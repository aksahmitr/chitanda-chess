mod board;
mod lookup;

fn main() {
    let cur =
        board::Board::from_fen("rnbqkbnr/pp2pppp/8/3p4/2p1P3/2PP4/PP3PPP/RNBQKBNR w KQkq - 0 4")
            .unwrap();

    println!("{:#?}", cur.get_moves(board::PlayerColor::Black));
}
