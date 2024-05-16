mod board;
mod lookup;

fn main() {
    let cur =
        board::Board::from_fen("rnbqkbnr/pp2pppp/8/3p4/2p1P1P1/2PP4/PP3P1P/RNBQKBNR b KQkq g3 0 4")
            .unwrap();

    println!("{:#?}", cur.get_moves(board::PlayerColor::Black));
}
