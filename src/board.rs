use core::fmt;
use std::ops::Index;

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
        let file = iter.next().unwrap().to_lowercase().next().unwrap() as u8 - 'a' as u8;
        let rank = iter
            .next()
            .unwrap()
            .to_digit(10)
            .ok_or(BoardError::InvalidSquare)? as u8;
        Ok(Square::new(file, rank - 1)?)
    }
}

#[derive(Debug)]
pub struct ChessMove {
    origin: Square,
    target: Square,
}

//make it index arrays of size 2
#[derive(PartialEq, Clone, Copy)]
pub enum PlayerColor {
    White,
    Black,
}

//make it index arrays of size 6
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

#[derive(Clone)]
pub struct Board {
    active_color: PlayerColor,

    white_castle_kingside: bool,
    white_castle_queenside: bool,
    black_castle_kingside: bool,
    black_castle_queenside: bool,

    en_passant_square: Option<Square>,

    half_move_clock: u32,
    full_move_clock: u32,

    pub white: Bitboard,
    pub black: Bitboard,

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

fn get_high_mask(x: u64) -> u64 {
    if x > 0 {
        (1 << x.trailing_zeros()) - 1
    } else {
        !0
    }
}

fn get_low_mask(x: u64) -> u64 {
    if x > 0 {
        //need more efficient way
        let y = 1 << (63 - x.leading_zeros());
        !((y - 1) | y)
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
        self.remove_piece(square);
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

    fn remove_piece(&mut self, square: Square) {
        let pos: u64 = !(1 << square as u8);
        self.pawn.0 &= pos;
        self.knight.0 &= pos;
        self.bishop.0 &= pos;
        self.rook.0 &= pos;
        self.queen.0 &= pos;
        self.king.0 &= pos;
        self.white.0 &= pos;
        self.black.0 &= pos;
    }

    fn get_piece(&self, square: Square) -> Option<(Piece, PlayerColor)> {
        let pos: u64 = 1 << square as u8;
        let color = if pos & self.white.0 > 0 {
            PlayerColor::White
        } else {
            PlayerColor::Black
        };
        if self.pawn.0 & pos > 0 {
            Some((Piece::Pawn, color))
        } else if self.knight.0 & pos > 0 {
            Some((Piece::Knight, color))
        } else if self.bishop.0 & pos > 0 {
            Some((Piece::Bishop, color))
        } else if self.rook.0 & pos > 0 {
            Some((Piece::Rook, color))
        } else if self.queen.0 & pos > 0 {
            Some((Piece::Queen, color))
        } else if self.king.0 & pos > 0 {
            Some((Piece::King, color))
        } else {
            None
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

    fn get_knight_moves(
        &self,
        moves: &mut Vec<ChessMove>,
        player_mask: Bitboard,
        enemy_mask: Bitboard,
    ) {
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
    }

    fn get_king_moves(
        &self,
        moves: &mut Vec<ChessMove>,
        player_mask: Bitboard,
        enemy_mask: Bitboard,
    ) {
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
    }

    fn get_rook_moves(
        &self,
        moves: &mut Vec<ChessMove>,
        player_mask: Bitboard,
        enemy_mask: Bitboard,
    ) {
        let mut pieces = self.rook.0 & player_mask.0;
        while pieces > 0 {
            let shift = pieces.trailing_zeros() as u8;
            let occupied = player_mask.0 | enemy_mask.0;
            pieces ^= 1 << shift;

            for id in [0, 2] {
                let origin = Square::from_id(shift).unwrap();

                let blockers = lookup::RAY_MOVES[id][origin] & occupied;

                let mut mask = lookup::RAY_MOVES[id][origin] & get_low_mask(blockers);

                if blockers > 0 {
                    mask |= (1 << (63 - blockers.leading_zeros())) & enemy_mask.0;
                }

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

                let mut mask = lookup::RAY_MOVES[id][origin] & get_high_mask(blockers);

                if blockers > 0 {
                    mask |= (1 << blockers.trailing_zeros()) & enemy_mask.0;
                }

                while mask > 0 {
                    let shift = mask.trailing_zeros() as u8;
                    mask ^= 1 << shift;

                    let target = Square::from_id(shift).unwrap();

                    moves.push(ChessMove { origin, target });
                }
            }
        }
    }

    fn get_bishop_moves(
        &self,
        moves: &mut Vec<ChessMove>,
        player_mask: Bitboard,
        enemy_mask: Bitboard,
    ) {
        let mut pieces = self.bishop.0 & player_mask.0;
        while pieces > 0 {
            let shift = pieces.trailing_zeros() as u8;
            let occupied = player_mask.0 | enemy_mask.0;
            pieces ^= 1 << shift;

            for id in [1, 3] {
                let origin = Square::from_id(shift).unwrap();

                let blockers = lookup::RAY_MOVES[id][origin] & occupied;

                let mut mask = lookup::RAY_MOVES[id][origin] & get_low_mask(blockers);

                if blockers > 0 {
                    mask |= (1 << (63 - blockers.leading_zeros())) & enemy_mask.0;
                }

                while mask > 0 {
                    let shift = mask.trailing_zeros() as u8;
                    mask ^= 1 << shift;

                    let target = Square::from_id(shift).unwrap();

                    moves.push(ChessMove { origin, target });
                }
            }

            for id in [5, 7] {
                let origin = Square::from_id(shift).unwrap();

                let blockers = lookup::RAY_MOVES[id][origin] & occupied;

                let mut mask = lookup::RAY_MOVES[id][origin] & get_high_mask(blockers);

                if blockers > 0 {
                    mask |= (1 << blockers.trailing_zeros()) & enemy_mask.0;
                }

                while mask > 0 {
                    let shift = mask.trailing_zeros() as u8;
                    mask ^= 1 << shift;

                    let target = Square::from_id(shift).unwrap();

                    moves.push(ChessMove { origin, target });
                }
            }
        }
    }

    fn get_queen_moves(
        &self,
        moves: &mut Vec<ChessMove>,
        player_mask: Bitboard,
        enemy_mask: Bitboard,
    ) {
        let mut pieces = self.queen.0 & player_mask.0;
        while pieces > 0 {
            let shift = pieces.trailing_zeros() as u8;
            let occupied = player_mask.0 | enemy_mask.0;
            pieces ^= 1 << shift;

            for id in 0..4 {
                let origin = Square::from_id(shift).unwrap();

                let blockers = lookup::RAY_MOVES[id][origin] & occupied;

                let mut mask = lookup::RAY_MOVES[id][origin] & get_low_mask(blockers);

                if blockers > 0 {
                    mask |= (1 << (63 - blockers.leading_zeros())) & enemy_mask.0;
                }

                while mask > 0 {
                    let shift = mask.trailing_zeros() as u8;
                    mask ^= 1 << shift;

                    let target = Square::from_id(shift).unwrap();

                    moves.push(ChessMove { origin, target });
                }
            }

            for id in 4..8 {
                let origin = Square::from_id(shift).unwrap();

                let blockers = lookup::RAY_MOVES[id][origin] & occupied;

                let mut mask = lookup::RAY_MOVES[id][origin] & get_high_mask(blockers);

                if blockers > 0 {
                    mask |= (1 << blockers.trailing_zeros()) & enemy_mask.0;
                }

                while mask > 0 {
                    let shift = mask.trailing_zeros() as u8;
                    mask ^= 1 << shift;

                    let target = Square::from_id(shift).unwrap();

                    moves.push(ChessMove { origin, target });
                }
            }
        }
    }

    fn get_pawn_moves(
        &self,
        moves: &mut Vec<ChessMove>,
        player_mask: Bitboard,
        enemy_mask: Bitboard,
        color: PlayerColor,
    ) {
        let pawns = player_mask.0 & self.pawn.0;
        let en_passant_mask = self
            .en_passant_square
            .map_or(0, |square| ((1 as u64) << square as u8));
        let enemy_mask = enemy_mask.0 | en_passant_mask;
        if color == PlayerColor::White {
            //single push
            let mut pieces = pawns;
            while pieces > 0 {
                let shift = pieces.trailing_zeros() as u8;
                let occupied = player_mask.0 | enemy_mask;
                let origin = Square::from_id(shift).unwrap();

                //will break if on final rank

                let target = Square::from_id(shift - 8).unwrap();
                if ((1 << shift) >> 8) & occupied == 0 {
                    moves.push(ChessMove { origin, target });
                }

                pieces ^= 1 << shift;
            }

            //double push
            let mut pieces = pawns & 0xFF000000000000;
            while pieces > 0 {
                let shift = pieces.trailing_zeros() as u8;
                let occupied = player_mask.0 | enemy_mask;
                let origin = Square::from_id(shift).unwrap();

                //will break if on final rank
                let target = Square::from_id(shift - 16).unwrap();

                if (((1 << shift) >> 8) | ((1 << shift) >> 16)) & occupied == 0 {
                    moves.push(ChessMove { origin, target });
                }

                pieces ^= 1 << shift;
            }

            //left capture

            let mut pieces = pawns & 0x7F7F7F7F7F7F7F7F;
            while pieces > 0 {
                let shift = pieces.trailing_zeros() as u8;
                let origin = Square::from_id(shift).unwrap();

                //will break if on final rank
                let target = Square::from_id(shift - 9).unwrap();

                if (1 << (shift - 9)) & enemy_mask > 0 {
                    moves.push(ChessMove { origin, target });
                }

                pieces ^= 1 << shift;
            }

            //right capture

            let mut pieces = pawns & 0xFEFEFEFEFEFEFEFE;
            while pieces > 0 {
                let shift = pieces.trailing_zeros() as u8;
                let origin = Square::from_id(shift).unwrap();

                //will break if on final rank
                let target = Square::from_id(shift - 7).unwrap();

                if (1 << (shift - 7)) & enemy_mask > 0 {
                    moves.push(ChessMove { origin, target });
                }

                pieces ^= 1 << shift;
            }
        } else {
            //single push
            let mut pieces = pawns;
            while pieces > 0 {
                let shift = pieces.trailing_zeros() as u8;
                let occupied = player_mask.0 | enemy_mask;
                let origin = Square::from_id(shift).unwrap();

                //will break if on final rank

                let target = Square::from_id(shift + 8).unwrap();
                if ((1 << shift) << 8) & occupied == 0 {
                    moves.push(ChessMove { origin, target });
                }

                pieces ^= 1 << shift;
            }

            //double push
            let mut pieces = pawns & 0xFF00;
            while pieces > 0 {
                let shift = pieces.trailing_zeros() as u8;
                let occupied = player_mask.0 | enemy_mask;
                let origin = Square::from_id(shift).unwrap();

                //will break if on final rank
                let target = Square::from_id(shift + 16).unwrap();

                if (((1 << shift) << 8) | ((1 << shift) << 16)) & occupied == 0 {
                    moves.push(ChessMove { origin, target });
                }

                pieces ^= 1 << shift;
            }

            //left capture

            let mut pieces = pawns & 0x7F7F7F7F7F7F7F7F;
            while pieces > 0 {
                let shift = pieces.trailing_zeros() as u8;
                let origin = Square::from_id(shift).unwrap();

                //will break if on final rank
                let target = Square::from_id(shift + 7).unwrap();

                if (1 << (shift + 7)) & enemy_mask > 0 {
                    moves.push(ChessMove { origin, target });
                }

                pieces ^= 1 << shift;
            }

            //right capture

            let mut pieces = pawns & 0xFEFEFEFEFEFEFEFE;
            while pieces > 0 {
                let shift = pieces.trailing_zeros() as u8;
                let origin = Square::from_id(shift).unwrap();

                //will break if on final rank
                let target = Square::from_id(shift + 9).unwrap();

                if (1 << (shift + 9)) & enemy_mask > 0 {
                    moves.push(ChessMove { origin, target });
                }

                pieces ^= 1 << shift;
            }
        }
    }

    pub fn is_attacked(&self, square: Square, color: PlayerColor) -> bool {
        let player_mask: Bitboard;
        let enemy_mask: Bitboard;
        if color == PlayerColor::White {
            player_mask = self.white;
            enemy_mask = self.black;
        } else {
            player_mask = self.black;
            enemy_mask = self.white;
        }
        let occupied = player_mask.0 | enemy_mask.0;
        if lookup::KING_MOVES[square as usize] & enemy_mask.0 & self.king.0 > 0 {
            return true;
        };

        if lookup::KNIGHT_MOVES[square as usize] & enemy_mask.0 & self.knight.0 > 0 {
            return true;
        };

        for id in [0, 2] {
            let blockers = lookup::RAY_MOVES[id][square as usize] & occupied;

            if (1 << (63 - blockers.leading_zeros())) & enemy_mask.0 & (self.rook.0 | self.queen.0)
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

            if (1 << blockers.trailing_zeros()) & enemy_mask.0 & (self.rook.0 | self.queen.0) > 0 {
                return true;
            }
        }

        for id in [1, 3] {
            let blockers = lookup::RAY_MOVES[id][square as usize] & occupied;

            if blockers == 0 {
                continue;
            }

            if (1 << (63 - blockers.leading_zeros()))
                & enemy_mask.0
                & (self.bishop.0 | self.queen.0)
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

            if (1 << blockers.trailing_zeros()) & enemy_mask.0 & (self.bishop.0 | self.queen.0) > 0
            {
                return true;
            }
        }

        //pawn checks

        let mut mask: u64 = 0;

        if color == PlayerColor::White {
            if square as u8 >= 9 {
                mask |= 1 << (square as u8 - 9);
            }
            if square as u8 >= 7 {
                mask |= 1 << (square as u8 - 7);
            }
        } else {
            if square as u8 + 9 < 64 {
                mask |= 1 << (square as u8 + 9);
            }
            if square as u8 + 7 < 64 {
                mask |= 1 << (square as u8 + 7);
            }
        }

        if mask & enemy_mask.0 & self.pawn.0 > 0 {
            return true;
        }
        false
    }

    pub fn is_in_check(&self, color: PlayerColor) -> bool {
        let player_mask;
        let enemy_mask;
        if color == PlayerColor::White {
            player_mask = self.white;
            enemy_mask = self.black;
        } else {
            player_mask = self.black;
            enemy_mask = self.white;
        }

        let mut pieces = self.king.0 & player_mask.0;
        while pieces > 0 {
            let shift = pieces.trailing_zeros() as u8;
            pieces ^= 1 << shift;
            if self.is_attacked(Square::from_id(shift).unwrap(), color) {
                return true;
            }
        }
        false
    }

    fn can_castle_kingside(&self, color: PlayerColor) -> bool {
        if color == PlayerColor::White {
            !(self.is_attacked(Square::E1, color)
                || self.is_attacked(Square::F1, color)
                || self.is_attacked(Square::G1, color))
        } else {
            !(self.is_attacked(Square::E8, color)
                || self.is_attacked(Square::F8, color)
                || self.is_attacked(Square::G8, color))
        }
    }

    fn can_castle_queenside(&self, color: PlayerColor) -> bool {
        if color == PlayerColor::White {
            !(self.is_attacked(Square::E1, color)
                || self.is_attacked(Square::D1, color)
                || self.is_attacked(Square::C1, color))
        } else {
            !(self.is_attacked(Square::E8, color)
                || self.is_attacked(Square::D8, color)
                || self.is_attacked(Square::C8, color))
        }
    }

    pub fn get_moves(&self, color: PlayerColor) -> Vec<ChessMove> {
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

        self.get_knight_moves(&mut moves, player_mask, enemy_mask);
        self.get_king_moves(&mut moves, player_mask, enemy_mask);
        self.get_rook_moves(&mut moves, player_mask, enemy_mask);
        self.get_bishop_moves(&mut moves, player_mask, enemy_mask);
        self.get_queen_moves(&mut moves, player_mask, enemy_mask);
        self.get_pawn_moves(&mut moves, player_mask, enemy_mask, color);

        moves
    }

    pub fn make_move(&mut self, chess_move: ChessMove) {
        //does not consider en passant and castling
        let piece = self.get_piece(chess_move.origin).unwrap();
        self.set_piece(piece.0, piece.1, chess_move.target);
        self.remove_piece(chess_move.origin);
    }

    pub fn is_legal_move(&self, chess_move: ChessMove) -> bool {
        //does not consider en passant and castling
        let mut new_board = self.clone();
        let piece = self.get_piece(chess_move.origin).unwrap();
        new_board.set_piece(piece.0, piece.1, chess_move.target);
        new_board.remove_piece(chess_move.origin);
        !new_board.is_in_check(piece.1)
    }
}
