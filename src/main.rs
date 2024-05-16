mod board;
mod lookup;

fn main() {
    let cur =
        board::Board::from_fen("rnbqkbnr/p1pppppp/8/1p6/4P3/7P/PPPP1PP1/RNBQKBNR b KQkq - 0 2")
            .unwrap();

    println!("{:#?}", cur.get_moves(board::PlayerColor::Black));
}
