//! Mimimal implementation of shogi position representation.
//! Implemented based on shogi_core crate.

use std::fmt::Write;
use std::ops::{BitAnd, BitOr, BitXor, Not};

trait ToUsi {
    fn to_usi<W: Write>(&self, sink: &mut W) -> std::fmt::Result;
}

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

impl Color {
    const fn index(&self) -> usize {
        match self {
            Color::Black => 0,
            Color::White => 1,
        }
    }

    fn from_index(index: usize) -> Self {
        unsafe { std::mem::transmute(index as u8) }
    }
}

impl ToUsi for Color {
    fn to_usi<W: Write>(&self, sink: &mut W) -> std::fmt::Result {
        match self {
            Color::Black => write!(sink, "b"),
            Color::White => write!(sink, "w"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
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
    pub const fn index(&self) -> usize {
        *self as usize
    }

    pub fn from_index(index: usize) -> Self {
        unsafe { std::mem::transmute(index as u8) }
    }
}

#[derive(Clone, Copy)]
pub struct Piece {
    value: u8,
}

#[allow(dead_code)]
pub const P_B_PAWN: Piece = Piece::new(Color::Black, PieceKind::Pawn);
#[allow(dead_code)]
pub const P_B_LANCE: Piece = Piece::new(Color::Black, PieceKind::Lance);
#[allow(dead_code)]
pub const P_B_KNIGHT: Piece = Piece::new(Color::Black, PieceKind::Knight);
#[allow(dead_code)]
pub const P_B_SILVER: Piece = Piece::new(Color::Black, PieceKind::Silver);
#[allow(dead_code)]
pub const P_B_BISHOP: Piece = Piece::new(Color::Black, PieceKind::Bishop);
#[allow(dead_code)]
pub const P_B_ROOK: Piece = Piece::new(Color::Black, PieceKind::Rook);
#[allow(dead_code)]
pub const P_B_GOLD: Piece = Piece::new(Color::Black, PieceKind::Gold);
#[allow(dead_code)]
pub const P_B_KING: Piece = Piece::new(Color::Black, PieceKind::King);
#[allow(dead_code)]
pub const P_B_PRO_PAWN: Piece = Piece::new(Color::Black, PieceKind::ProPawn);
#[allow(dead_code)]
pub const P_B_PRO_LANCE: Piece = Piece::new(Color::Black, PieceKind::ProLance);
#[allow(dead_code)]
pub const P_B_PRO_KNIGHT: Piece = Piece::new(Color::Black, PieceKind::ProKnight);
#[allow(dead_code)]
pub const P_B_PRO_SILVER: Piece = Piece::new(Color::Black, PieceKind::ProSilver);
#[allow(dead_code)]
pub const P_B_PRO_BISHOP: Piece = Piece::new(Color::Black, PieceKind::ProBishop);
#[allow(dead_code)]
pub const P_B_PRO_ROOK: Piece = Piece::new(Color::Black, PieceKind::ProRook);
#[allow(dead_code)]
pub const P_W_PAWN: Piece = Piece::new(Color::White, PieceKind::Pawn);
#[allow(dead_code)]
pub const P_W_LANCE: Piece = Piece::new(Color::White, PieceKind::Lance);
#[allow(dead_code)]
pub const P_W_KNIGHT: Piece = Piece::new(Color::White, PieceKind::Knight);
#[allow(dead_code)]
pub const P_W_SILVER: Piece = Piece::new(Color::White, PieceKind::Silver);
#[allow(dead_code)]
pub const P_W_BISHOP: Piece = Piece::new(Color::White, PieceKind::Bishop);
#[allow(dead_code)]
pub const P_W_ROOK: Piece = Piece::new(Color::White, PieceKind::Rook);
#[allow(dead_code)]
pub const P_W_GOLD: Piece = Piece::new(Color::White, PieceKind::Gold);
#[allow(dead_code)]
pub const P_W_KING: Piece = Piece::new(Color::White, PieceKind::King);
#[allow(dead_code)]
pub const P_W_PRO_PAWN: Piece = Piece::new(Color::White, PieceKind::ProPawn);
#[allow(dead_code)]
pub const P_W_PRO_LANCE: Piece = Piece::new(Color::White, PieceKind::ProLance);
#[allow(dead_code)]
pub const P_W_PRO_KNIGHT: Piece = Piece::new(Color::White, PieceKind::ProKnight);
#[allow(dead_code)]
pub const P_W_PRO_SILVER: Piece = Piece::new(Color::White, PieceKind::ProSilver);
#[allow(dead_code)]
pub const P_W_PRO_BISHOP: Piece = Piece::new(Color::White, PieceKind::ProBishop);
#[allow(dead_code)]
pub const P_W_PRO_ROOK: Piece = Piece::new(Color::White, PieceKind::ProRook);

impl Piece {
    pub const fn new(color: Color, kind: PieceKind) -> Self {
        let kind_index = kind.index() as u8;
        let color_index = color.index() as u8;
        Piece {
            value: kind_index | (color_index << 4),
        }
    }

    pub fn kind(&self) -> PieceKind {
        let kind_index = (self.value & 0x0F) as usize;
        PieceKind::from_index(kind_index)
    }

    pub fn color(&self) -> Color {
        let color_index = (self.value >> 4) as usize;
        Color::from_index(color_index)
    }
}

impl ToUsi for Piece {
    fn to_usi<W: Write>(&self, sink: &mut W) -> std::fmt::Result {
        const PIECE_STRS: [[&str; 2]; 14] = [
            ["P", "p"],
            ["L", "l"],
            ["N", "n"],
            ["S", "s"],
            ["B", "b"],
            ["R", "r"],
            ["G", "g"],
            ["K", "k"],
            ["+P", "+p"],
            ["+L", "+l"],
            ["+N", "+n"],
            ["+S", "+s"],
            ["+B", "+b"],
            ["+R", "+r"],
        ];
        let kind_index = self.kind().index();
        let color_index = self.color().index();
        write!(sink, "{}", PIECE_STRS[kind_index][color_index])
    }
}

// Hand representation: [Pawn, Lance, Knight, Silver, Bishop, Rook, Gold, padding]
#[derive(Clone, Copy)]
#[repr(align(8))]
pub struct Hand(pub [u8; 8]);

impl ToUsi for [Hand; 2] {
    fn to_usi<W: Write>(&self, sink: &mut W) -> std::fmt::Result {
        const PIECE_STRS: [[&str; 2]; 7] = [
            ["P", "p"],
            ["L", "l"],
            ["N", "n"],
            ["S", "s"],
            ["B", "b"],
            ["R", "r"],
            ["G", "g"],
        ];
        let mut is_empty = true;
        for color in 0..2 {
            for kind in 0..7 {
                let count = self[color].0[kind];
                for _ in 0..count {
                    write!(sink, "{}", PIECE_STRS[kind][color])?;
                    is_empty = false;
                }
            }
        }
        if is_empty {
            write!(sink, "-")?;
        }
        Ok(())
    }
}

#[derive(Clone, Copy)]
pub struct Square(pub u8);

pub struct Position {
    board: [Option<Piece>; 81],
    hands: [Hand; 2],
    player_bb: [Bitboard; 2],
    piece_bb: [Bitboard; 14],
    king_square: [Square; 2],
    side_to_move: Color,
    ply: u32,
}

impl Position {
    pub fn new(
        board: [Option<Piece>; 81],
        hands: [Hand; 2],
        side_to_move: Color,
        ply: u32,
    ) -> Self {
        let mut player_bb = [Bitboard(0, 0); 2];
        let mut piece_bb = [Bitboard(0, 0); 14];
        let mut king_square = [Square(0); 2];
        for (i, square) in board.iter().enumerate() {
            if let Some(piece) = square {
                let color_index = piece.color().index();
                let kind_index = piece.kind().index();
                let bit = 1u64.wrapping_shl(i as u32 % 63);
                if i < 63 {
                    player_bb[color_index].0 |= bit;
                    piece_bb[kind_index].0 |= bit;
                } else {
                    player_bb[color_index].1 |= bit;
                    piece_bb[kind_index].1 |= bit;
                }
                if piece.kind() == PieceKind::King {
                    king_square[color_index] = Square(i as u8);
                }
            }
        }
        Position {
            board,
            hands,
            player_bb,
            piece_bb,
            king_square,
            side_to_move,
            ply,
        }
    }

    fn to_sfen<W: Write>(&self, sink: &mut W) -> std::fmt::Result {
        for i in 0..9 {
            let mut vacant = 0;
            for j in 0..9 {
                // Safety: the index is in range 0..81.
                let current = &self.board[9 * (8 - j) + i];
                if let Some(occupying) = current {
                    if vacant > 0 {
                        write!(sink, "{}", vacant)?;
                        vacant = 0;
                    }
                    occupying.to_usi(sink)?;
                } else {
                    vacant += 1;
                }
            }
            if vacant > 0 {
                write!(sink, "{}", vacant)?;
            }
            if i < 8 {
                // Safety: '/' is in ASCII
                write!(sink, "/")?;
            }
        }
        // Safety: ' ' is in ASCII
        write!(sink, " ")?;
        self.side_to_move.to_usi(sink)?;
        // Safety: ' ' is in ASCII
        write!(sink, " ")?;
        self.hands.to_usi(sink)?;
        // Safety: ' ' is in ASCII
        write!(sink, " ")?;
        write!(sink, "{}", self.ply)?;
        Ok(())
    }

    pub fn to_sfen_owned(&self) -> String {
        let mut sfen = String::new();
        self.to_sfen(&mut sfen).unwrap();
        sfen
    }

    pub fn player_bb(&self, color: Color) -> Bitboard {
        self.player_bb[color.index()]
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
        self.king_square[color.index()]
    }

    pub fn hand(&self, color: Color) -> &Hand {
        &self.hands[color.index()]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_position_to_sfen() {
        let empty_board = [None; 81];
        let hands = [Hand([0; 8]), Hand([0; 8])];
        let position = Position::new(empty_board, hands, Color::Black, 1);
        let sfen = position.to_sfen_owned();
        assert_eq!(sfen, "9/9/9/9/9/9/9/9/9 b - 1");
    }
}
