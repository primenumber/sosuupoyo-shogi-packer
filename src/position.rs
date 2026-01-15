//! Mimimal implementation of shogi position representation.
//! Implemented based on shogi_core crate.

use std::fmt::Write;
use std::ops::{BitAnd, BitOr, BitXor, Not};

trait ToUsi {
    fn to_usi<W: Write>(&self, sink: &mut W) -> std::fmt::Result;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(align(16))]
pub struct Bitboard(pub u64, pub u64);

const MASK_81: Bitboard = Bitboard(0x7FFF_FFFF_FFFF_FFFF, 0x3_FFFF);

impl Bitboard {
    pub fn bitnot_raw(self) -> Bitboard {
        Bitboard(!self.0, !self.1)
    }

    pub fn from_square(square: Square) -> Self {
        let square = square.0 as i32;
        if square < 63 {
            Bitboard(1u64 << square, 0)
        } else {
            Bitboard(0, 1u64 << (square - 63))
        }
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

    pub fn from_index(index: usize) -> Self {
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Square(pub u8);

#[derive(Debug, PartialEq, Eq)]
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

    pub fn startpos() -> Self {
        // Create initial position (SFEN: lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1)
        let mut board = [None; 81];

        // 1st rank: White's back rank
        board[0] = Some(P_W_LANCE);
        board[9] = Some(P_W_KNIGHT);
        board[18] = Some(P_W_SILVER);
        board[27] = Some(P_W_GOLD);
        board[36] = Some(P_W_KING);
        board[45] = Some(P_W_GOLD);
        board[54] = Some(P_W_SILVER);
        board[63] = Some(P_W_KNIGHT);
        board[72] = Some(P_W_LANCE);

        // 2nd rank: White's rook and bishop
        board[10] = Some(P_W_BISHOP);
        board[64] = Some(P_W_ROOK);

        // 3ed rank: White's pawns
        for i in 0..9 {
            board[i * 9 + 2] = Some(P_W_PAWN);
        }

        // 7th rank: Black's pawns
        for i in 0..9 {
            board[i * 9 + 6] = Some(P_B_PAWN);
        }

        // 8th rank: Black's bishop and rook
        board[16] = Some(P_B_ROOK);
        board[70] = Some(P_B_BISHOP);

        // 9th rank: Black's back rank
        board[8] = Some(P_B_LANCE);
        board[17] = Some(P_B_KNIGHT);
        board[26] = Some(P_B_SILVER);
        board[35] = Some(P_B_GOLD);
        board[44] = Some(P_B_KING);
        board[53] = Some(P_B_GOLD);
        board[62] = Some(P_B_SILVER);
        board[71] = Some(P_B_KNIGHT);
        board[80] = Some(P_B_LANCE);

        let hands = [Hand([0; 8]), Hand([0; 8])];
        Position::new(board, hands, Color::Black, 1)
    }

    pub fn set_only_kings() -> Self {
        let mut board = [None; 81];
        board[36] = Some(P_W_KING);
        board[44] = Some(P_B_KING);
        // All other pieces are hand pieces
        let hands = [
            Hand([9, 2, 2, 2, 1, 1, 2, 0]),
            Hand([9, 2, 2, 2, 1, 1, 2, 0]),
        ];
        Position::new(board, hands, Color::Black, 1)
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

    pub fn board(&self) -> &[Option<Piece>; 81] {
        &self.board
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

    /// Construct a Position from bitboards instead of board array.
    /// This is the inverse of extracting player_bb and piece_bb from a Position.
    pub fn from_bitboards(
        player_bb: [Bitboard; 2],
        piece_bb: [Bitboard; 14],
        hands: [Hand; 2],
        side_to_move: Color,
        ply: u32,
    ) -> Self {
        let mut board = [None; 81];
        let mut king_square = [Square(0); 2];

        // For each square, determine the piece (if any)
        for sq in 0..81 {
            let (word_idx, bit) = if sq < 63 {
                (0, 1u64 << sq)
            } else {
                (1, 1u64 << (sq - 63))
            };

            // Check which player owns a piece on this square
            let color_opt = if word_idx == 0 {
                if player_bb[0].0 & bit != 0 {
                    Some(Color::Black)
                } else if player_bb[1].0 & bit != 0 {
                    Some(Color::White)
                } else {
                    None
                }
            } else {
                if player_bb[0].1 & bit != 0 {
                    Some(Color::Black)
                } else if player_bb[1].1 & bit != 0 {
                    Some(Color::White)
                } else {
                    None
                }
            };

            if let Some(color) = color_opt {
                // Find which piece kind is on this square
                for kind_idx in 0..14 {
                    let has_piece = if word_idx == 0 {
                        piece_bb[kind_idx].0 & bit != 0
                    } else {
                        piece_bb[kind_idx].1 & bit != 0
                    };
                    if has_piece {
                        let kind = PieceKind::from_index(kind_idx);
                        let piece = Piece::new(color, kind);
                        board[sq] = Some(piece);
                        if kind == PieceKind::King {
                            king_square[color.index()] = Square(sq as u8);
                        }
                        break;
                    }
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

    #[test]
    fn test_from_bitboards_empty() {
        let empty_board = [None; 81];
        let hands = [Hand([0; 8]), Hand([0; 8])];
        let original = Position::new(empty_board, hands, Color::Black, 1);

        let reconstructed = Position::from_bitboards(
            original.player_bb,
            original.piece_bb,
            original.hands,
            original.side_to_move,
            original.ply,
        );

        assert_eq!(original, reconstructed);
    }

    #[test]
    fn test_from_bitboards_with_pieces() {
        // Create a board with some pieces
        let mut board = [None; 81];
        // Black king at 5i (index 4)
        board[4] = Some(P_B_KING);
        // White king at 5a (index 76)
        board[76] = Some(P_W_KING);
        // Black pawn at 5g (index 22)
        board[22] = Some(P_B_PAWN);
        // White rook at 8b (index 70)
        board[70] = Some(P_W_ROOK);
        // Black promoted bishop at 3c (index 56)
        board[56] = Some(P_B_PRO_BISHOP);

        let hands = [
            Hand([1, 0, 0, 0, 0, 0, 2, 0]),
            Hand([0, 1, 0, 0, 0, 0, 0, 0]),
        ];
        let original = Position::new(board, hands, Color::White, 42);

        let reconstructed = Position::from_bitboards(
            original.player_bb,
            original.piece_bb,
            original.hands,
            original.side_to_move,
            original.ply,
        );

        assert_eq!(original, reconstructed);
    }

    #[test]
    fn test_from_bitboards_startpos() {
        let original = Position::startpos();
        let reconstructed = Position::from_bitboards(
            original.player_bb,
            original.piece_bb,
            original.hands,
            original.side_to_move,
            original.ply,
        );

        assert_eq!(original, reconstructed);
    }
}
