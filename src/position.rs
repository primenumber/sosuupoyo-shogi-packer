//! Mimimal implementation of shogi position representation.
//! Implemented based on shogi_core crate.

use std::ops::{BitAnd, BitOr, BitXor, Not};

#[derive(Clone, Copy)]
#[repr(align(16))]
pub struct Bitboard(pub u64, pub u64);

const MASK_81: Bitboard = Bitboard(0x7FFF_FFFF_FFFF_FFFF, 0x3_FFFF);

impl Bitboard {
    pub fn bitnot_raw(self) -> Bitboard {
        Bitboard(!self.0, !self.1)
    }
}

impl BitAnd for Bitboard {
    type Output = Bitboard;

    fn bitand(self, rhs: Bitboard) -> Bitboard {
        Bitboard(self.0 & rhs.0, self.1 & rhs.1)
    }
}

impl BitOr for Bitboard {
    type Output = Bitboard;

    fn bitor(self, rhs: Bitboard) -> Bitboard {
        Bitboard(self.0 | rhs.0, self.1 | rhs.1)
    }
}

impl BitXor for Bitboard {
    type Output = Bitboard;

    fn bitxor(self, rhs: Bitboard) -> Bitboard {
        Bitboard(self.0 ^ rhs.0, self.1 ^ rhs.1)
    }
}

impl Not for Bitboard {
    type Output = Bitboard;

    fn not(self) -> Bitboard {
        self.bitnot_raw() & MASK_81
    }
}

#[derive(Clone, Copy)]
pub enum Color {
    Black,
    White,
}

#[derive(Clone, Copy)]
pub enum PieceKind {
    Pawn,
    Lance,
    Knight,
    Silver,
    Bishop,
    Rook,
    Gold,
    King,
    ProPawn,
    ProLance,
    ProKnight,
    ProSilver,
    ProBishop, // a.k.a. Horse
    ProRook,   // a.k.a. Dragon
}

impl PieceKind {
    pub fn index(&self) -> usize {
        *self as usize
    }
}

pub struct Piece {
    kind: PieceKind,
    color: Color,
}

// Hand representation: [Pawn, Lance, Knight, Silver, Bishop, Rook, Gold, padding]
#[derive(Clone, Copy)]
#[repr(align(8))]
pub struct Hand(pub [u8; 8]);

#[derive(Clone, Copy)]
pub struct Square(pub u8);

pub struct Position {
    hands: [Hand; 2],
    player_bb: [Bitboard; 2],
    piece_bb: [Bitboard; 14],
    king_square: [Square; 2],
    side_to_move: Color,
}

impl Position {
    pub fn new(
        hands: [Hand; 2],
        player_bb: [Bitboard; 2],
        piece_bb: [Bitboard; 14],
        king_square: [Square; 2],
        side_to_move: Color,
    ) -> Self {
        Position {
            hands,
            player_bb,
            piece_bb,
            king_square,
            side_to_move,
        }
    }

    pub fn player_bb(&self, color: Color) -> Bitboard {
        match color {
            Color::Black => self.player_bb[0],
            Color::White => self.player_bb[1],
        }
    }

    pub fn occupied_bitboard(&self) -> Bitboard {
        let mut bb = Bitboard(0, 0);
        for piece_bb in &self.piece_bb {
            bb.0 |= piece_bb.0;
            bb.1 |= piece_bb.1;
        }
        bb
    }

    pub fn piece_kind_bitboard(&self, kind: PieceKind) -> Bitboard {
        let index = kind.index();
        self.piece_bb[index]
    }

    pub fn side_to_move(&self) -> Color {
        self.side_to_move
    }

    pub fn king_square(&self, color: Color) -> Square {
        match color {
            Color::Black => self.king_square[0],
            Color::White => self.king_square[1],
        }
    }

    pub fn hand(&self, color: Color) -> &Hand {
        match color {
            Color::Black => &self.hands[0],
            Color::White => &self.hands[1],
        }
    }
}
