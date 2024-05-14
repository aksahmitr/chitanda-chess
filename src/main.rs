mod board;
mod lookup;

fn main() {
    let cur =
        board::Board::from_fen("r1b1kbnr/pp1ppppp/2p5/1q6/3nP3/5N1P/PPP1BPP1/RNBQK2R w KQkq - 1 6")
            .unwrap();

    println!("{:#?}", cur.get_moves(board::PlayerColor::White));
}
