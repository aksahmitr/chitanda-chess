use crate::board::Board;
use crate::board::PlayerColor;

impl Board {
    pub fn eval(&self) -> i32 {
        let mut eval: i32 = 0;

        for (index, value) in [1, 3, 3, 5, 9, 10000].iter().enumerate() {
            eval += value
                * (2 * ((self.color_board[PlayerColor::White] & self.piece_board[index])
                    .count_ones())
                    - self.piece_board[index].count_ones()) as i32;
        }
        eval
    }
}
