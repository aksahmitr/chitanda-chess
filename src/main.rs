mod board;
mod lookup;

fn main() {
    let cur = board::Board::from_fen(
        "r1b1kbnr/pp1p1ppp/q1p5/2R1p3/P3P3/5N1P/1Pn1BPP1/1NBQ1K1R w kq e6 0 11",
    )
    .unwrap();

    println!("{:#?}", cur.get_moves(board::PlayerColor::White));
}
