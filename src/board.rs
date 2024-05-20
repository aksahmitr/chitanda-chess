use core::fmt;
use std::ops::{Index, IndexMut};

use crate::lookup;

#[derive(Debug, PartialEq, Clone)]
pub enum CastlingState {
    WhiteKingSide,
    WhiteQueenSide,
    BlackKingSide,
    BlackQueenSide,
}

impl Index<CastlingState> for [bool; 4] {
    type Output = bool;
    fn index(&self, index: CastlingState) -> &Self::Output {
        &self[index as usize]
    }
}

impl IndexMut<CastlingState> for [bool; 4] {
    fn index_mut(&mut self, index: CastlingState) -> &mut Self::Output {
        &mut self[index as usize]
    }
}

#[rustfmt::skip]
#[derive(Clone,Copy,Debug, PartialEq)]
pub enum Square {
    A8, B8, C8, D8, E8, F8, G8, H8,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A1, B1, C1, D1, E1, F1, G1, H1,
}

impl Index<Square> for [u64; 64] {
    type Output = u64;

    fn index(&self, index: Square) -> &Self::Output {
        &self[index as usize]
    }
}

impl TryFrom<u8> for Square {
    type Error = BoardError;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        //replace with match statement later

        use Square::*;

        #[rustfmt::skip]
        const LOOKUP: [Square; 64] = [
            A8, B8, C8, D8, E8, F8, G8, H8,
            A7, B7, C7, D7, E7, F7, G7, H7,
            A6, B6, C6, D6, E6, F6, G6, H6,
            A5, B5, C5, D5, E5, F5, G5, H5,
            A4, B4, C4, D4, E4, F4, G4, H4,
            A3, B3, C3, D3, E3, F3, G3, H3,
            A2, B2, C2, D2, E2, F2, G2, H2,
            A1, B1, C1, D1, E1, F1, G1, H1,
        ];

        LOOKUP
            .get(value as usize)
            .ok_or(BoardError::OutOfBoundAccess)
            .copied()
    }
}

impl Square {
    pub fn new(file: u8, rank: u8) -> Result<Square, BoardError> {
        let pos: u8 = 8 * (7 - rank) + file;

        Square::try_from(pos)
    }

    pub fn from_algebraic_notation(square: &str) -> Result<Square, BoardError> {
        if square.len() != 2 {
            return Err(BoardError::InvalidSquare);
        }
        let mut iter = square.chars();
        let file = iter.next().unwrap().to_lowercase().next().unwrap() as u8 - 'a' as u8;
        let rank = iter
            .next()
            .unwrap()
            .to_digit(10)
            .ok_or(BoardError::InvalidSquare)? as u8;
        Ok(Square::new(file, rank - 1)?)
    }
}

//add moved piece here
#[derive(Debug, PartialEq, Clone)]
pub struct ChessMove {
    pub origin: Square,
    pub target: Square,
    pub promotion_piece: Option<Piece>,
    pub castle_type: Option<CastlingState>,
}

//make it index arrays of size 2
#[derive(PartialEq, Clone, Copy, Debug)]
pub enum PlayerColor {
    White,
    Black,
}

impl TryFrom<u8> for PlayerColor {
    type Error = BoardError;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(PlayerColor::White),
            1 => Ok(PlayerColor::Black),
            _ => Err(BoardError::InvalidColor),
        }
    }
}

impl TryFrom<usize> for Piece {
    type Error = BoardError;
    fn try_from(value: usize) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Piece::Pawn),
            1 => Ok(Piece::Knight),
            2 => Ok(Piece::Bishop),
            3 => Ok(Piece::Rook),
            4 => Ok(Piece::Queen),
            5 => Ok(Piece::King),
            _ => Err(BoardError::InvalidPiece),
        }
    }
}

impl PlayerColor {
    pub fn other(&self) -> PlayerColor {
        PlayerColor::try_from(1 ^ (*self as u8)).unwrap()
    }
}

impl Index<PlayerColor> for [u64; 2] {
    type Output = u64;
    fn index(&self, index: PlayerColor) -> &Self::Output {
        &self[index as usize]
    }
}

impl IndexMut<PlayerColor> for [u64; 2] {
    fn index_mut(&mut self, index: PlayerColor) -> &mut Self::Output {
        &mut self[index as usize]
    }
}

impl Index<Piece> for [u64; 6] {
    type Output = u64;
    fn index(&self, index: Piece) -> &Self::Output {
        &self[index as usize]
    }
}
impl IndexMut<Piece> for [u64; 6] {
    fn index_mut(&mut self, index: Piece) -> &mut Self::Output {
        &mut self[index as usize]
    }
}
//make it index arrays of size 6
#[derive(PartialEq, Clone, Copy, Debug)]
pub enum Piece {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

// impl fmt::Display for u64 {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         for i in 0..8 {
//             for j in 0..8 {
//                 let pos: u64 = 1u64 << (8 * i + j);
//                 write!(f, "{} ", if self & pos > 0 { 1 } else { 0 })?;
//             }
//             write!(f, "\n")?;
//         }
//         Ok(())
//     }
// }

#[derive(Debug, Clone, PartialEq)]
struct DeltaBoard {
    castling_valid: [bool; 4],
    en_passant_square: Option<Square>,
    captured_piece: Option<Piece>,
    moved_piece: Piece,
    chess_move: ChessMove,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Board {
    pub active_color: PlayerColor,

    castling_valid: [bool; 4],

    en_passant_square: Option<Square>,

    half_move_clock: u32,
    full_move_clock: u32,

    color_board: [u64; 2],

    piece_board: [u64; 6],
    //delta: Vec<DeltaBoard>,
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "White : \n")?;
        for i in 0..8 {
            for j in 0..8 {
                let pos: u64 = 1u64 << (8 * i + j);
                write!(
                    f,
                    "{} ",
                    if self.color_board[PlayerColor::White] & pos > 0 {
                        1
                    } else {
                        0
                    }
                )?;
            }
            write!(f, "\n")?;
        }
        write!(f, "Black : \n")?;
        for i in 0..8 {
            for j in 0..8 {
                let pos: u64 = 1u64 << (8 * i + j);
                write!(
                    f,
                    "{} ",
                    if self.color_board[PlayerColor::Black] & pos > 0 {
                        1
                    } else {
                        0
                    }
                )?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum BoardError {
    SquareNotEmpty,
    InvalidFEN,
    OutOfBoundAccess,
    InvalidSquare,
    ParseError(std::num::ParseIntError),
    InvalidColor,
    InvalidPiece,
}

impl fmt::Display for BoardError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BoardError::InvalidFEN => write!(f, "Invalid FEN"),
            BoardError::SquareNotEmpty => write!(f, "Square being accessed already has a piece"),
            BoardError::OutOfBoundAccess => write!(f, "Out of bounds square being accessed"),
            BoardError::InvalidSquare => write!(f, "Invalid square being accessed"),
            BoardError::ParseError(err) => write!(f, "Parsing error: {}", err),
            BoardError::InvalidColor => write!(f, "Invalid color being accessed"),
            BoardError::InvalidPiece => write!(f, "Invalid piece being accessed"),
        }
    }
}

impl std::error::Error for BoardError {}

impl From<std::num::ParseIntError> for BoardError {
    fn from(value: std::num::ParseIntError) -> Self {
        BoardError::ParseError(value)
    }
}

fn get_high_mask(x: u64) -> u64 {
    if x > 0 {
        (1u64 << x.trailing_zeros()) - 1
    } else {
        !0
    }
}

fn get_low_mask(x: u64) -> u64 {
    if x > 0 {
        //need more efficient way
        let y = 1u64 << (63 - x.leading_zeros());
        !((y - 1) | y)
    } else {
        !0
    }
}

impl Board {
    pub fn new() -> Board {
        Board {
            active_color: PlayerColor::White,

            castling_valid: [false; 4],

            en_passant_square: None,

            half_move_clock: 0,
            full_move_clock: 0,

            color_board: [0; 2],

            piece_board: [0; 6],
            //delta: Vec::new(),
        }
    }

    fn set_piece(&mut self, piece: Piece, color: PlayerColor, square: Square) {
        self.remove_any_piece(square);
        let pos: u64 = 1u64 << square as u8;
        self.piece_board[piece] |= pos;
        self.color_board[color] |= pos;
    }

    fn remove_any_piece(&mut self, square: Square) {
        let pos: u64 = !(1u64 << square as u8);
        for board in self.piece_board.iter_mut() {
            *board &= pos;
        }
        for board in self.color_board.iter_mut() {
            *board &= pos;
        }
    }

    fn remove_piece(&mut self, square: Square, piece: Piece) {
        let pos: u64 = !(1u64 << square as u8);
        self.piece_board[piece] &= pos;
    }

    fn get_piece(&self, square: Square) -> Option<(Piece, PlayerColor)> {
        let pos: u64 = 1u64 << square as u8;
        let color = if pos & self.color_board[PlayerColor::White] > 0 {
            PlayerColor::White
        } else {
            PlayerColor::Black
        };
        for piece in 0..6 {
            if self.piece_board[piece] & pos > 0 {
                return Some((Piece::try_from(piece).unwrap(), color));
            }
        }
        None
    }

    pub fn from_fen(fen: &str) -> Result<Board, BoardError> {
        let mut board = Board::new();
        let mut iter = fen.split_ascii_whitespace();

        let piece_placement = iter.next().ok_or(BoardError::InvalidFEN)?;
        let active_color = iter.next().ok_or(BoardError::InvalidFEN)?;
        let castling_availability = iter.next().ok_or(BoardError::InvalidFEN)?;
        let en_passant_square = iter.next().ok_or(BoardError::InvalidFEN)?;
        let half_move_clock = iter.next().ok_or(BoardError::InvalidFEN)?;
        let full_move_clock = iter.next().ok_or(BoardError::InvalidFEN)?;

        let mut row_iter = piece_placement.split("/");

        for rank in (0..8).rev() {
            let row = row_iter.next().ok_or(BoardError::InvalidFEN)?;
            let mut file: u8 = 0;

            for c in row.chars() {
                if let Some(digit) = c.to_digit(10) {
                    file += digit as u8;
                } else {
                    board.set_piece(
                        match c.to_lowercase().next().unwrap_or('.') {
                            'p' => Piece::Pawn,
                            'n' => Piece::Knight,
                            'b' => Piece::Bishop,
                            'r' => Piece::Rook,
                            'q' => Piece::Queen,
                            'k' => Piece::King,
                            _ => return Err(BoardError::InvalidFEN),
                        },
                        if c.is_uppercase() {
                            PlayerColor::White
                        } else {
                            PlayerColor::Black
                        },
                        Square::new(file, rank)?,
                    );
                    file += 1;
                }
            }
        }

        board.active_color = match active_color {
            "w" => PlayerColor::White,
            "b" => PlayerColor::Black,
            _ => return Err(BoardError::InvalidFEN),
        };

        board.castling_valid[CastlingState::WhiteKingSide] = castling_availability.contains("K");
        board.castling_valid[CastlingState::WhiteQueenSide] = castling_availability.contains("Q");
        board.castling_valid[CastlingState::BlackKingSide] = castling_availability.contains("k");
        board.castling_valid[CastlingState::BlackKingSide] = castling_availability.contains("q");

        board.en_passant_square = match en_passant_square {
            "-" => None,
            square => Some(Square::from_algebraic_notation(square)?),
        };

        board.half_move_clock = half_move_clock.parse::<u32>()?;
        board.full_move_clock = full_move_clock.parse::<u32>()?;

        Ok(board)
    }

    fn get_knight_moves(&self, moves: &mut Vec<ChessMove>, player_mask: u64, enemy_mask: u64) {
        let mut pieces = self.piece_board[Piece::Knight] & player_mask;
        while pieces > 0 {
            let shift = pieces.trailing_zeros() as u8;
            pieces ^= 1u64 << shift;

            let origin = Square::try_from(shift).unwrap();
            let mut mask = lookup::KNIGHT_MOVES[origin] & !player_mask;

            while mask > 0 {
                let shift = mask.trailing_zeros() as u8;
                mask ^= 1u64 << shift;

                let target = Square::try_from(shift).unwrap();

                moves.push(ChessMove {
                    origin,
                    target,
                    promotion_piece: None,
                    castle_type: None,
                });
            }
        }
    }

    fn get_king_moves(&self, moves: &mut Vec<ChessMove>, player_mask: u64, enemy_mask: u64) {
        let mut pieces = self.piece_board[Piece::King] & player_mask;
        while pieces > 0 {
            let shift = pieces.trailing_zeros() as u8;
            pieces ^= 1u64 << shift;

            let origin = Square::try_from(shift).unwrap();
            let mut mask = lookup::KING_MOVES[origin] & !player_mask;

            while mask > 0 {
                let shift = mask.trailing_zeros() as u8;
                mask ^= 1u64 << shift;

                let target = Square::try_from(shift).unwrap();

                moves.push(ChessMove {
                    origin,
                    target,
                    promotion_piece: None,
                    castle_type: None,
                });
            }
        }

        if self.active_color == PlayerColor::White {
            if self.can_castle(CastlingState::WhiteKingSide) {
                moves.push(ChessMove {
                    origin: Square::E1,
                    target: Square::G1,
                    promotion_piece: None,
                    castle_type: Some(CastlingState::WhiteKingSide),
                });
            }
            if self.can_castle(CastlingState::WhiteQueenSide) {
                moves.push(ChessMove {
                    origin: Square::E1,
                    target: Square::C1,
                    promotion_piece: None,
                    castle_type: Some(CastlingState::WhiteQueenSide),
                });
            }
        } else {
            if self.can_castle(CastlingState::BlackKingSide) {
                moves.push(ChessMove {
                    origin: Square::E8,
                    target: Square::G8,
                    promotion_piece: None,
                    castle_type: Some(CastlingState::BlackKingSide),
                });
            }
            if self.can_castle(CastlingState::BlackQueenSide) {
                moves.push(ChessMove {
                    origin: Square::E8,
                    target: Square::C8,
                    promotion_piece: None,
                    castle_type: Some(CastlingState::BlackQueenSide),
                });
            }
        }
    }

    fn get_rook_moves(&self, moves: &mut Vec<ChessMove>, player_mask: u64, enemy_mask: u64) {
        let mut pieces = self.piece_board[Piece::Rook] & player_mask;
        while pieces > 0 {
            let shift = pieces.trailing_zeros() as u8;
            let occupied = player_mask | enemy_mask;
            pieces ^= 1u64 << shift;

            for id in [0, 2] {
                let origin = Square::try_from(shift).unwrap();

                let blockers = lookup::RAY_MOVES[id][origin] & occupied;

                let mut mask = lookup::RAY_MOVES[id][origin] & get_low_mask(blockers);

                if blockers > 0 {
                    mask |= (1u64 << (63 - blockers.leading_zeros())) & enemy_mask;
                }

                while mask > 0 {
                    let shift = mask.trailing_zeros() as u8;
                    mask ^= 1u64 << shift;

                    let target = Square::try_from(shift).unwrap();

                    moves.push(ChessMove {
                        origin,
                        target,
                        promotion_piece: None,
                        castle_type: None,
                    });
                }
            }

            for id in [4, 6] {
                let origin = Square::try_from(shift).unwrap();

                let blockers = lookup::RAY_MOVES[id][origin] & occupied;

                let mut mask = lookup::RAY_MOVES[id][origin] & get_high_mask(blockers);

                if blockers > 0 {
                    mask |= (1u64 << blockers.trailing_zeros()) & enemy_mask;
                }

                while mask > 0 {
                    let shift = mask.trailing_zeros() as u8;
                    mask ^= 1u64 << shift;

                    let target = Square::try_from(shift).unwrap();

                    moves.push(ChessMove {
                        origin,
                        target,
                        promotion_piece: None,
                        castle_type: None,
                    });
                }
            }
        }
    }

    fn get_bishop_moves(&self, moves: &mut Vec<ChessMove>, player_mask: u64, enemy_mask: u64) {
        let mut pieces = self.piece_board[Piece::Bishop] & player_mask;
        while pieces > 0 {
            let shift = pieces.trailing_zeros() as u8;
            let occupied = player_mask | enemy_mask;
            pieces ^= 1u64 << shift;

            for id in [1, 3] {
                let origin = Square::try_from(shift).unwrap();

                let blockers = lookup::RAY_MOVES[id][origin] & occupied;

                let mut mask = lookup::RAY_MOVES[id][origin] & get_low_mask(blockers);

                if blockers > 0 {
                    mask |= (1u64 << (63 - blockers.leading_zeros())) & enemy_mask;
                }

                while mask > 0 {
                    let shift = mask.trailing_zeros() as u8;
                    mask ^= 1u64 << shift;

                    let target = Square::try_from(shift).unwrap();

                    moves.push(ChessMove {
                        origin,
                        target,
                        promotion_piece: None,
                        castle_type: None,
                    });
                }
            }

            for id in [5, 7] {
                let origin = Square::try_from(shift).unwrap();

                let blockers = lookup::RAY_MOVES[id][origin] & occupied;

                let mut mask = lookup::RAY_MOVES[id][origin] & get_high_mask(blockers);

                if blockers > 0 {
                    mask |= (1u64 << blockers.trailing_zeros()) & enemy_mask;
                }

                while mask > 0 {
                    let shift = mask.trailing_zeros() as u8;
                    mask ^= 1u64 << shift;

                    let target = Square::try_from(shift).unwrap();

                    moves.push(ChessMove {
                        origin,
                        target,
                        promotion_piece: None,
                        castle_type: None,
                    });
                }
            }
        }
    }

    fn get_queen_moves(&self, moves: &mut Vec<ChessMove>, player_mask: u64, enemy_mask: u64) {
        let mut pieces = self.piece_board[Piece::Queen] & player_mask;
        while pieces > 0 {
            let shift = pieces.trailing_zeros() as u8;
            let occupied = player_mask | enemy_mask;
            pieces ^= 1u64 << shift;

            for id in 0..4 {
                let origin = Square::try_from(shift).unwrap();

                let blockers = lookup::RAY_MOVES[id][origin] & occupied;

                let mut mask = lookup::RAY_MOVES[id][origin] & get_low_mask(blockers);

                if blockers > 0 {
                    mask |= (1u64 << (63 - blockers.leading_zeros())) & enemy_mask;
                }

                while mask > 0 {
                    let shift = mask.trailing_zeros() as u8;
                    mask ^= 1u64 << shift;

                    let target = Square::try_from(shift).unwrap();

                    moves.push(ChessMove {
                        origin,
                        target,
                        promotion_piece: None,
                        castle_type: None,
                    });
                }
            }

            for id in 4..8 {
                let origin = Square::try_from(shift).unwrap();

                let blockers = lookup::RAY_MOVES[id][origin] & occupied;

                let mut mask = lookup::RAY_MOVES[id][origin] & get_high_mask(blockers);

                if blockers > 0 {
                    mask |= (1u64 << blockers.trailing_zeros()) & enemy_mask;
                }

                while mask > 0 {
                    let shift = mask.trailing_zeros() as u8;
                    mask ^= 1u64 << shift;

                    let target = Square::try_from(shift).unwrap();

                    moves.push(ChessMove {
                        origin,
                        target,
                        promotion_piece: None,
                        castle_type: None,
                    });
                }
            }
        }
    }

    fn get_pawn_moves(&self, moves: &mut Vec<ChessMove>, player_mask: u64, enemy_mask: u64) {
        let pawns = player_mask & self.piece_board[Piece::Pawn];
        let en_passant_mask = self
            .en_passant_square
            .map_or(0, |square| ((1 as u64) << square as u8));
        let enemy_mask = enemy_mask | en_passant_mask;
        if self.active_color == PlayerColor::White {
            //single push
            let mut pieces = pawns;
            while pieces > 0 {
                let shift = pieces.trailing_zeros() as u8;
                let occupied = player_mask | enemy_mask;
                let origin = Square::try_from(shift).unwrap();

                //will break if on final rank

                let target = Square::try_from(shift - 8).unwrap();
                if ((1u64 << shift) >> 8) & occupied == 0 {
                    if shift - 8 < 8 {
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Queen),
                            castle_type: None,
                        });
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Rook),
                            castle_type: None,
                        });
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Knight),
                            castle_type: None,
                        });
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Bishop),
                            castle_type: None,
                        });
                    } else {
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: None,
                            castle_type: None,
                        });
                    }
                }

                pieces ^= 1u64 << shift;
            }

            //double push
            let mut pieces = pawns & 0xFF000000000000;
            while pieces > 0 {
                let shift = pieces.trailing_zeros() as u8;
                let occupied = player_mask | enemy_mask;
                let origin = Square::try_from(shift).unwrap();

                //will break if on final rank
                let target = Square::try_from(shift - 16).unwrap();

                if (((1u64 << shift) >> 8) | ((1u64 << shift) >> 16)) & occupied == 0 {
                    moves.push(ChessMove {
                        origin,
                        target,
                        promotion_piece: None,
                        castle_type: None,
                    });
                }

                pieces ^= 1u64 << shift;
            }

            //left capture

            let mut pieces = pawns & 0xFEFEFEFEFEFEFEFE;
            while pieces > 0 {
                let shift = pieces.trailing_zeros() as u8;
                let origin = Square::try_from(shift).unwrap();

                //will break if on final rank
                let target = Square::try_from(shift - 9).unwrap();

                if (1u64 << (shift - 9)) & enemy_mask > 0 {
                    if shift - 8 < 8 {
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Queen),
                            castle_type: None,
                        });
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Rook),
                            castle_type: None,
                        });
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Knight),
                            castle_type: None,
                        });
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Bishop),
                            castle_type: None,
                        });
                    } else {
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: None,
                            castle_type: None,
                        });
                    }
                }

                pieces ^= 1u64 << shift;
            }

            //right capture
            let mut pieces = pawns & 0x7F7F7F7F7F7F7F7F;
            while pieces > 0 {
                let shift = pieces.trailing_zeros() as u8;
                let origin = Square::try_from(shift).unwrap();

                //will break if on final rank
                let target = Square::try_from(shift - 7).unwrap();

                if (1u64 << (shift - 7)) & enemy_mask > 0 {
                    if shift - 8 < 8 {
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Queen),
                            castle_type: None,
                        });
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Rook),
                            castle_type: None,
                        });
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Knight),
                            castle_type: None,
                        });
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Bishop),
                            castle_type: None,
                        });
                    } else {
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: None,
                            castle_type: None,
                        });
                    }
                }

                pieces ^= 1u64 << shift;
            }
        } else {
            //single push
            let mut pieces = pawns;
            while pieces > 0 {
                let shift = pieces.trailing_zeros() as u8;
                let occupied = player_mask | enemy_mask;
                let origin = Square::try_from(shift).unwrap();

                //will break if on final rank

                let target = Square::try_from(shift + 8).unwrap();
                if ((1u64 << shift) << 8) & occupied == 0 {
                    if shift + 8 > 55 {
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Queen),
                            castle_type: None,
                        });
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Rook),
                            castle_type: None,
                        });
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Knight),
                            castle_type: None,
                        });
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Bishop),
                            castle_type: None,
                        });
                    } else {
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: None,
                            castle_type: None,
                        });
                    }
                }

                pieces ^= 1u64 << shift;
            }

            //double push
            let mut pieces = pawns & 0xFF00;
            while pieces > 0 {
                let shift = pieces.trailing_zeros() as u8;
                let occupied = player_mask | enemy_mask;
                let origin = Square::try_from(shift).unwrap();

                //will break if on final rank
                let target = Square::try_from(shift + 16).unwrap();

                if (((1u64 << shift) << 8) | ((1u64 << shift) << 16)) & occupied == 0 {
                    if shift + 8 > 55 {
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Queen),
                            castle_type: None,
                        });
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Rook),
                            castle_type: None,
                        });
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Knight),
                            castle_type: None,
                        });
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Bishop),
                            castle_type: None,
                        });
                    } else {
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: None,
                            castle_type: None,
                        });
                    }
                }

                pieces ^= 1u64 << shift;
            }

            //left capture

            let mut pieces = pawns & 0xFEFEFEFEFEFEFEFE;
            while pieces > 0 {
                let shift = pieces.trailing_zeros() as u8;
                let origin = Square::try_from(shift).unwrap();

                //will break if on final rank
                let target = Square::try_from(shift + 7).unwrap();

                if (1u64 << (shift + 7)) & enemy_mask > 0 {
                    if shift + 8 > 55 {
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Queen),
                            castle_type: None,
                        });
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Rook),
                            castle_type: None,
                        });
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Knight),
                            castle_type: None,
                        });
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Bishop),
                            castle_type: None,
                        });
                    } else {
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: None,
                            castle_type: None,
                        });
                    }
                }

                pieces ^= 1u64 << shift;
            }

            //right capture

            let mut pieces = pawns & 0x7F7F7F7F7F7F7F7F;
            while pieces > 0 {
                let shift = pieces.trailing_zeros() as u8;
                let origin = Square::try_from(shift).unwrap();

                //will break if on final rank
                let target = Square::try_from(shift + 9).unwrap();

                if (1u64 << (shift + 9)) & enemy_mask > 0 {
                    if shift + 8 > 55 {
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Queen),
                            castle_type: None,
                        });
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Rook),
                            castle_type: None,
                        });
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Knight),
                            castle_type: None,
                        });
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: Some(Piece::Bishop),
                            castle_type: None,
                        });
                    } else {
                        moves.push(ChessMove {
                            origin,
                            target,
                            promotion_piece: None,
                            castle_type: None,
                        });
                    }
                }

                pieces ^= 1u64 << shift;
            }
        }
    }

    pub fn is_attacked(&self, square: Square, color: PlayerColor) -> bool {
        let player_mask: u64 = self.color_board[color];
        let enemy_mask: u64 = self.color_board[color.other()];
        let occupied = player_mask | enemy_mask;
        if lookup::KING_MOVES[square as usize] & enemy_mask & self.piece_board[Piece::King] > 0 {
            return true;
        };

        if lookup::KNIGHT_MOVES[square as usize] & enemy_mask & self.piece_board[Piece::Knight] > 0
        {
            return true;
        };

        for id in [0, 2] {
            let blockers = lookup::RAY_MOVES[id][square as usize] & occupied;

            if blockers == 0 {
                continue;
            }

            if (1u64 << (63 - blockers.leading_zeros()))
                & enemy_mask
                & (self.piece_board[Piece::Rook] | self.piece_board[Piece::Queen])
                > 0
            {
                return true;
            }
        }

        for id in [4, 6] {
            let blockers = lookup::RAY_MOVES[id][square as usize] & occupied;

            if blockers == 0 {
                continue;
            }

            if (1u64 << blockers.trailing_zeros())
                & enemy_mask
                & (self.piece_board[Piece::Rook] | self.piece_board[Piece::Queen])
                > 0
            {
                return true;
            }
        }

        for id in [1, 3] {
            let blockers = lookup::RAY_MOVES[id][square as usize] & occupied;

            if blockers == 0 {
                continue;
            }

            if (1u64 << (63 - blockers.leading_zeros()))
                & enemy_mask
                & (self.piece_board[Piece::Bishop] | self.piece_board[Piece::Queen])
                > 0
            {
                return true;
            }
        }

        for id in [5, 7] {
            let blockers = lookup::RAY_MOVES[id][square as usize] & occupied;

            if blockers == 0 {
                continue;
            }

            if (1u64 << blockers.trailing_zeros())
                & enemy_mask
                & (self.piece_board[Piece::Bishop] | self.piece_board[Piece::Queen])
                > 0
            {
                return true;
            }
        }

        //pawn checks

        let mut mask: u64 = 0;

        if color == PlayerColor::White {
            if square as u8 >= 9 && ((square as u8 & 0b111) > 0) {
                mask |= 1u64 << (square as u8 - 9);
            }
            if square as u8 >= 7 && (((square as u8 + 1) & 0b111) > 0) {
                mask |= 1u64 << (square as u8 - 7);
            }
        } else {
            if square as u8 + 9 < 64 && (((square as u8 + 1) & 0b111) > 0) {
                mask |= 1u64 << (square as u8 + 9);
            }
            if square as u8 + 7 < 64 && ((square as u8 & 0b111) > 0) {
                mask |= 1u64 << (square as u8 + 7);
            }
        }

        if mask & enemy_mask & self.piece_board[Piece::Pawn] > 0 {
            return true;
        }
        false
    }

    pub fn is_in_check(&self, color: PlayerColor) -> bool {
        let player_mask = self.color_board[color];
        let enemy_mask = self.color_board[color.other()];

        let mut pieces = self.piece_board[Piece::King] & player_mask;
        while pieces > 0 {
            let shift = pieces.trailing_zeros() as u8;
            pieces ^= 1u64 << shift;
            if self.is_attacked(Square::try_from(shift).unwrap(), color) {
                return true;
            }
        }
        false
    }

    pub fn can_castle(&self, state: CastlingState) -> bool {
        if !self.castling_valid[state.clone()] {
            return false;
        }
        match state {
            CastlingState::WhiteKingSide => {
                !(self.is_attacked(Square::E1, PlayerColor::White)
                    || self.is_attacked(Square::F1, PlayerColor::White)
                    || self.is_attacked(Square::G1, PlayerColor::White))
                    && ((self.color_board[PlayerColor::White]
                        | self.color_board[PlayerColor::Black])
                        & 0x6000000000000000
                        == 0)
            }
            CastlingState::BlackKingSide => {
                !(self.is_attacked(Square::E8, PlayerColor::Black)
                    || self.is_attacked(Square::F8, PlayerColor::White)
                    || self.is_attacked(Square::G8, PlayerColor::White))
                    && ((self.color_board[PlayerColor::White]
                        | self.color_board[PlayerColor::Black])
                        & 0x60
                        == 0)
            }
            CastlingState::WhiteQueenSide => {
                !(self.is_attacked(Square::E1, PlayerColor::White)
                    || self.is_attacked(Square::D1, PlayerColor::White)
                    || self.is_attacked(Square::C1, PlayerColor::White))
                    && ((self.color_board[PlayerColor::White]
                        | self.color_board[PlayerColor::Black])
                        & 0xE00000000000000
                        == 0)
            }
            CastlingState::BlackQueenSide => {
                !(self.is_attacked(Square::E8, PlayerColor::Black)
                    || self.is_attacked(Square::D8, PlayerColor::Black)
                    || self.is_attacked(Square::C8, PlayerColor::Black))
                    && ((self.color_board[PlayerColor::White]
                        | self.color_board[PlayerColor::Black])
                        & 0xE
                        == 0)
            }
        }
    }

    pub fn get_moves(&self) -> Vec<ChessMove> {
        let player_mask = self.color_board[self.active_color];
        let enemy_mask = self.color_board[self.active_color.other()];

        let mut moves = Vec::new();

        self.get_knight_moves(&mut moves, player_mask, enemy_mask);
        self.get_king_moves(&mut moves, player_mask, enemy_mask);
        self.get_rook_moves(&mut moves, player_mask, enemy_mask);
        self.get_bishop_moves(&mut moves, player_mask, enemy_mask);
        self.get_queen_moves(&mut moves, player_mask, enemy_mask);
        self.get_pawn_moves(&mut moves, player_mask, enemy_mask);

        moves
    }

    pub fn make_move(&mut self, chess_move: ChessMove) {
        let piece = self.get_piece(chess_move.origin).unwrap();

        // let delta = DeltaBoard {
        //     castling_valid: self.castling_valid,
        //     en_passant_square: self.en_passant_square,
        //     captured_piece: self.get_piece(chess_move.target).map(|x| x.0),
        //     moved_piece: piece.0,
        //     chess_move: chess_move.clone(),
        // };

        // self.delta.push(delta);

        self.set_piece(piece.0, piece.1, chess_move.target);
        self.remove_any_piece(chess_move.origin);

        match chess_move.target {
            Square::A1 => {
                self.castling_valid[CastlingState::WhiteQueenSide] = false;
            }
            Square::H1 => {
                self.castling_valid[CastlingState::WhiteKingSide] = false;
            }
            Square::A8 => {
                self.castling_valid[CastlingState::BlackQueenSide] = false;
            }
            Square::H8 => {
                self.castling_valid[CastlingState::BlackKingSide] = false;
            }
            _ => (),
        }

        if piece.0 == Piece::Pawn {
            if let Some(promotion_piece) = chess_move.promotion_piece {
                self.set_piece(promotion_piece, piece.1, chess_move.target);
            }
            if let Some(en_passant_square) = self.en_passant_square {
                if en_passant_square == chess_move.target {
                    if piece.1 == PlayerColor::White {
                        self.remove_any_piece(
                            Square::try_from(chess_move.target as u8 + 8).unwrap(),
                        );
                    } else {
                        self.remove_any_piece(
                            Square::try_from(chess_move.target as u8 - 8).unwrap(),
                        );
                    }
                }
            }
            self.en_passant_square = None;
            if piece.1 == PlayerColor::White {
                if (chess_move.origin as i8 - chess_move.target as i8).abs() == 16 {
                    self.en_passant_square =
                        Some(Square::try_from(chess_move.origin as u8 - 8).unwrap());
                }
            } else {
                if (chess_move.origin as i8 - chess_move.target as i8).abs() == 16 {
                    self.en_passant_square =
                        Some(Square::try_from(chess_move.origin as u8 + 8).unwrap());
                }
            }
        } else {
            self.en_passant_square = None;
            if let Some(state) = chess_move.castle_type {
                match state {
                    CastlingState::WhiteKingSide => {
                        self.set_piece(Piece::Rook, PlayerColor::White, Square::F1);
                        self.remove_piece(Square::H1, Piece::Rook);
                    }
                    CastlingState::WhiteQueenSide => {
                        self.set_piece(Piece::Rook, PlayerColor::White, Square::D1);
                        self.remove_piece(Square::A1, Piece::Rook);
                    }
                    CastlingState::BlackKingSide => {
                        self.set_piece(Piece::Rook, PlayerColor::Black, Square::F8);
                        self.remove_piece(Square::H8, Piece::Rook);
                    }
                    CastlingState::BlackQueenSide => {
                        self.set_piece(Piece::Rook, PlayerColor::Black, Square::D8);
                        self.remove_piece(Square::A8, Piece::Rook);
                    }
                }
                self.castling_valid[CastlingState::WhiteKingSide] = false;
                self.castling_valid[CastlingState::WhiteQueenSide] = false;
                self.castling_valid[CastlingState::BlackKingSide] = false;
                self.castling_valid[CastlingState::BlackQueenSide] = false;
            } else if piece.0 == Piece::Rook {
                match chess_move.origin {
                    Square::A1 => {
                        self.castling_valid[CastlingState::WhiteQueenSide] = false;
                    }
                    Square::H1 => {
                        self.castling_valid[CastlingState::WhiteKingSide] = false;
                    }
                    Square::A8 => {
                        self.castling_valid[CastlingState::BlackQueenSide] = false;
                    }
                    Square::H8 => {
                        self.castling_valid[CastlingState::BlackKingSide] = false;
                    }
                    _ => (),
                }
            }
        }
        self.active_color = self.active_color.other();
    }

    // pub fn undo_move(&mut self) {
    //     let del = self.delta.pop().unwrap();
    //     self.active_color = self.active_color.other();
    //     self.castling_valid = del.castling_valid;
    //     self.en_passant_square = del.en_passant_square;
    //     self.set_piece(del.moved_piece, self.active_color, del.chess_move.origin);
    //     if let Some(captured_piece) = del.captured_piece {
    //         self.set_piece(
    //             captured_piece,
    //             self.active_color.other(),
    //             del.chess_move.target,
    //         );
    //     } else {
    //         self.remove_any_piece(del.chess_move.target);
    //     }

    //     if let Some(state) = del.chess_move.castle_type {
    //         match state {
    //             CastlingState::WhiteKingSide => {
    //                 self.set_piece(Piece::Rook, PlayerColor::White, Square::H1);
    //                 self.remove_piece(Square::F1, Piece::Rook);
    //             }
    //             CastlingState::WhiteQueenSide => {
    //                 self.set_piece(Piece::Rook, PlayerColor::White, Square::A1);
    //                 self.remove_piece(Square::D1, Piece::Rook);
    //             }
    //             CastlingState::BlackKingSide => {
    //                 self.set_piece(Piece::Rook, PlayerColor::Black, Square::H8);
    //                 self.remove_piece(Square::F8, Piece::Rook);
    //             }
    //             CastlingState::BlackQueenSide => {
    //                 self.set_piece(Piece::Rook, PlayerColor::Black, Square::A8);
    //                 self.remove_piece(Square::D8, Piece::Rook);
    //             }
    //         }
    //         return;
    //     }

    //     if let Some(en_passant_square) = self.en_passant_square {
    //         if del.moved_piece == Piece::Pawn && del.chess_move.target == en_passant_square {
    //             self.set_piece(
    //                 Piece::Pawn,
    //                 self.active_color.other(),
    //                 if self.active_color == PlayerColor::White {
    //                     (en_passant_square as u8 + 8).try_into().unwrap()
    //                 } else {
    //                     (en_passant_square as u8 - 8).try_into().unwrap()
    //                 },
    //             );
    //         }
    //     }
    // }
}
