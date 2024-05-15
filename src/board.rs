#![allow(dead_code, unused_variables)]

use core::fmt;
use std::{ops::Index, vec};

use crate::lookup;

#[rustfmt::skip]
#[derive(Clone,Copy,Debug)]
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
        let i = index as usize;
        &self[i]
    }
}

impl Square {
    pub fn new(file: u8, rank: u8) -> Result<Square, BoardError> {
        let pos: u8 = 8 * (7 - rank) + file;

        Square::from_id(pos)
    }

    pub fn from_id(id: u8) -> Result<Square, BoardError> {
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
            .get(id as usize)
            .ok_or(BoardError::OutOfBoundAccess)
            .copied()
    }

    pub fn from_algebraic_notation(square: &str) -> Result<Square, BoardError> {
        if square.len() != 2 {
            return Err(BoardError::InvalidSquare);
        }
        let mut iter = square.chars();
        let file = iter.next().unwrap() as u8 - 'A' as u8;
        let rank = iter
            .next()
            .unwrap()
            .to_digit(10)
            .ok_or(BoardError::InvalidSquare)? as u8;
        Ok(Square::new(file, rank)?)
    }
}

#[derive(Debug)]
pub struct ChessMove {
    origin: Square,
    target: Square,
}

#[derive(PartialEq)]
pub enum PlayerColor {
    White,
    Black,
}

pub enum Piece {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

#[derive(Clone, Copy)]
pub struct Bitboard(pub u64);

impl fmt::Display for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for i in 0..8 {
            for j in 0..8 {
                let pos: u64 = 1 << (8 * i + j);
                write!(f, "{} ", if self.0 & pos > 0 { 1 } else { 0 })?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

pub struct Board {
    active_color: PlayerColor,

    white_castle_kingside: bool,
    white_castle_queenside: bool,
    black_castle_kingside: bool,
    black_castle_queenside: bool,

    en_passant_square: Option<Square>,

    half_move_clock: u32,
    full_move_clock: u32,

    white: Bitboard,
    black: Bitboard,

    pawn: Bitboard,
    knight: Bitboard,
    bishop: Bitboard,
    rook: Bitboard,
    queen: Bitboard,
    king: Bitboard,
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "White : \n")?;
        for i in 0..8 {
            for j in 0..8 {
                let pos: u64 = 1 << (8 * i + j);
                write!(f, "{} ", if self.white.0 & pos > 0 { 1 } else { 0 })?;
            }
            write!(f, "\n")?;
        }
        write!(f, "Black : \n")?;
        for i in 0..8 {
            for j in 0..8 {
                let pos: u64 = 1 << (8 * i + j);
                write!(f, "{} ", if self.black.0 & pos > 0 { 1 } else { 0 })?;
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
}

impl fmt::Display for BoardError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BoardError::InvalidFEN => write!(f, "Invalid FEN"),
            BoardError::SquareNotEmpty => write!(f, "Square being accessed already has a piece"),
            BoardError::OutOfBoundAccess => write!(f, "Out of bounds square being accessed"),
            BoardError::InvalidSquare => write!(f, "Invalid square being accessed"),
            BoardError::ParseError(err) => write!(f, "Parsing error: {}", err),
        }
    }
}

impl std::error::Error for BoardError {}

impl From<std::num::ParseIntError> for BoardError {
    fn from(value: std::num::ParseIntError) -> Self {
        BoardError::ParseError(value)
    }
}

fn get_low_mask(x: u64) -> u64 {
    if x > 0 {
        (1 << x.trailing_zeros()) - 1
    } else {
        !0
    }
}

fn get_high_mask(x: u64) -> u64 {
    if x > 0 {
        !1 << x.trailing_zeros()
    } else {
        !0
    }
}

impl Board {
    pub fn new() -> Board {
        Board {
            active_color: PlayerColor::White,

            white_castle_kingside: false,
            white_castle_queenside: false,
            black_castle_kingside: false,
            black_castle_queenside: false,

            en_passant_square: None,

            half_move_clock: 0,
            full_move_clock: 0,

            white: Bitboard(0),
            black: Bitboard(0),
            pawn: Bitboard(0),
            knight: Bitboard(0),
            bishop: Bitboard(0),
            rook: Bitboard(0),
            queen: Bitboard(0),
            king: Bitboard(0),
        }
    }

    fn set_piece(&mut self, piece: Piece, player: PlayerColor, square: Square) {
        let pos: u64 = 1 << square as u8;
        match piece {
            Piece::Pawn => self.pawn.0 |= pos,
            Piece::Knight => self.knight.0 |= pos,
            Piece::Bishop => self.bishop.0 |= pos,
            Piece::Rook => self.rook.0 |= pos,
            Piece::Queen => self.queen.0 |= pos,
            Piece::King => self.king.0 |= pos,
        }
        match player {
            PlayerColor::White => self.white.0 |= pos,
            PlayerColor::Black => self.black.0 |= pos,
        }
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

        board.white_castle_kingside = castling_availability.contains("K");
        board.white_castle_queenside = castling_availability.contains("Q");
        board.black_castle_kingside = castling_availability.contains("k");
        board.black_castle_queenside = castling_availability.contains("q");

        board.en_passant_square = match en_passant_square {
            "-" => None,
            square => Some(Square::from_algebraic_notation(square)?),
        };

        board.half_move_clock = half_move_clock.parse::<u32>()?;
        board.full_move_clock = full_move_clock.parse::<u32>()?;

        Ok(board)
    }

    pub fn get_moves(&self, color: PlayerColor) -> Vec<ChessMove> {
        let occupied = self.white.0 | self.black.0;
        let empty = !occupied;

        let player_mask = if color == PlayerColor::White {
            self.white
        } else {
            self.black
        };

        let enemy_mask = if color == PlayerColor::White {
            self.black
        } else {
            self.white
        };

        let mut moves = Vec::new();

        //knight moves

        let mut pieces = self.knight.0 & player_mask.0;
        while pieces > 0 {
            let shift = pieces.trailing_zeros() as u8;
            pieces ^= 1 << shift;

            let origin = Square::from_id(shift).unwrap();
            let mut mask = lookup::KNIGHT_MOVES[origin] & !player_mask.0;

            while mask > 0 {
                let shift = mask.trailing_zeros() as u8;
                mask ^= 1 << shift;

                let target = Square::from_id(shift).unwrap();

                moves.push(ChessMove { origin, target });
            }
        }

        //king moves

        let mut pieces = self.king.0 & player_mask.0;
        while pieces > 0 {
            let shift = pieces.trailing_zeros() as u8;
            pieces ^= 1 << shift;

            let origin = Square::from_id(shift).unwrap();
            let mut mask = lookup::KING_MOVES[origin] & !player_mask.0;

            while mask > 0 {
                let shift = mask.trailing_zeros() as u8;
                mask ^= 1 << shift;

                let target = Square::from_id(shift).unwrap();

                moves.push(ChessMove { origin, target });
            }
        }

        //rook moves
        let mut pieces = self.rook.0 & player_mask.0;
        while pieces > 0 {
            let shift = pieces.trailing_zeros() as u8;
            pieces ^= 1 << shift;

            for id in [0, 2] {
                let origin = Square::from_id(shift).unwrap();

                let blockers = lookup::RAY_MOVES[id][origin] & occupied;

                let mut mask = lookup::RAY_MOVES[id][origin] & get_low_mask(blockers);

                while mask > 0 {
                    let shift = mask.trailing_zeros() as u8;
                    mask ^= 1 << shift;

                    let target = Square::from_id(shift).unwrap();

                    moves.push(ChessMove { origin, target });
                }
            }

            for id in [4, 6] {
                let origin = Square::from_id(shift).unwrap();

                let blockers = lookup::RAY_MOVES[id][origin] & occupied;

                let mut mask = (lookup::RAY_MOVES[id][origin]
                    & ((1 << blockers.trailing_zeros()) - 1))
                    | (blockers & enemy_mask.0);

                while mask > 0 {
                    let shift = mask.trailing_zeros() as u8;
                    mask ^= 1 << shift;

                    let target = Square::from_id(shift).unwrap();

                    moves.push(ChessMove { origin, target });
                }
            }
        }

        moves
    }
}
