//! Mimimal implementation of shogi position representation.
//! Implemented based on shogi_core crate.

use std::fmt::Write;
use std::ops::{BitAnd, BitOr, BitXor, Not};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SfenParseError {
    InvalidFormat,
    InvalidPiece(char),
    InvalidColor(char),
    InvalidHandPiece(char),
    InvalidPly,
}

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

    pub fn at(&self, square: Square) -> bool {
        let square = square.0 as i32;
        if square < 63 {
            (self.0 & (1u64 << square)) != 0
        } else {
            (self.1 & (1u64 << (square - 63))) != 0
        }
    }

    pub fn peek(&self) -> Option<Square> {
        if self.0 != 0 {
            let lsb = self.0 & self.0.wrapping_neg();
            let index = lsb.trailing_zeros();
            Some(Square(index as u8))
        } else if self.1 != 0 {
            let lsb = self.1 & self.1.wrapping_neg();
            let index = lsb.trailing_zeros();
            Some(Square((index + 63) as u8))
        } else {
            None
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

    /// Parse a piece from SFEN character.
    /// `c` is the piece character (P, L, N, S, B, R, G, K and lowercase variants).
    /// `promoted` indicates if the piece is promoted (preceded by '+').
    pub fn from_sfen(c: char, promoted: bool) -> Result<Self, SfenParseError> {
        let color = if c.is_ascii_uppercase() {
            Color::Black
        } else {
            Color::White
        };

        let base = match c.to_ascii_uppercase() {
            'P' => 0,
            'L' => 1,
            'N' => 2,
            'S' => 3,
            'B' => 4,
            'R' => 5,
            'G' => 6,
            'K' => 7,
            _ => return Err(SfenParseError::InvalidPiece(c)),
        };

        // Gold(6)とKing(7)は成れない
        if promoted && base >= 6 {
            return Err(SfenParseError::InvalidPiece(c));
        }

        let kind = PieceKind::from_index(if promoted { base + 8 } else { base });
        Ok(Piece::new(color, kind))
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
                if count >= 2 {
                    write!(sink, "{}", count)?;
                }
                if count >= 1 {
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
            hands,
            player_bb,
            piece_bb,
            king_square,
            side_to_move,
            ply,
        }
    }

    pub fn startpos() -> Self {
        Position::from_sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")
            .unwrap()
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
                let current = &self.at(Square((i * 9 + j) as u8));
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

    /// Parse a SFEN string into a Position.
    /// Format: "board side_to_move hand ply"
    /// Example: "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1"
    pub fn from_sfen(sfen: &str) -> Result<Self, SfenParseError> {
        let parts: Vec<&str> = sfen.split_whitespace().collect();
        if parts.len() != 4 {
            return Err(SfenParseError::InvalidFormat);
        }

        let board_str = parts[0];
        let side_str = parts[1];
        let hand_str = parts[2];
        let ply_str = parts[3];

        // Parse board
        let mut board = [None; 81];
        let ranks: Vec<&str> = board_str.split('/').collect();
        if ranks.len() != 9 {
            return Err(SfenParseError::InvalidFormat);
        }

        for (rank_idx, rank_str) in ranks.iter().enumerate() {
            let mut file = 8i32; // Start from file 9 (index 8) going to file 1 (index 0)
            let mut chars = rank_str.chars().peekable();

            while let Some(c) = chars.next() {
                if file < 0 {
                    return Err(SfenParseError::InvalidFormat);
                }

                if c.is_ascii_digit() {
                    // Empty squares
                    let num = c.to_digit(10).unwrap() as i32;
                    file -= num;
                } else if c == '+' {
                    // Promoted piece
                    let piece_char = chars.next().ok_or(SfenParseError::InvalidFormat)?;
                    let piece = Piece::from_sfen(piece_char, true)?;
                    let sq = file as usize * 9 + rank_idx;
                    board[sq] = Some(piece);
                    file -= 1;
                } else {
                    // Normal piece
                    let piece = Piece::from_sfen(c, false)?;
                    let sq = file as usize * 9 + rank_idx;
                    board[sq] = Some(piece);
                    file -= 1;
                }
            }
        }

        // Parse side to move
        let side_to_move = match side_str {
            "b" => Color::Black,
            "w" => Color::White,
            _ => {
                return Err(SfenParseError::InvalidColor(
                    side_str.chars().next().unwrap_or('?'),
                ))
            }
        };

        // Parse hand
        let hands = Self::parse_hand(hand_str)?;

        // Parse ply
        let ply: u32 = ply_str.parse().map_err(|_| SfenParseError::InvalidPly)?;

        Ok(Position::new(board, hands, side_to_move, ply))
    }

    fn parse_hand(hand_str: &str) -> Result<[Hand; 2], SfenParseError> {
        let mut hands = [Hand([0; 8]), Hand([0; 8])];

        if hand_str == "-" {
            return Ok(hands);
        }

        let mut chars = hand_str.chars().peekable();
        while let Some(c) = chars.next() {
            let mut count = 1u8;

            // Check if it's a digit (count prefix)
            if c.is_ascii_digit() {
                count = c.to_digit(10).unwrap() as u8;
                // Check for second digit (e.g., "18P")
                if let Some(&next) = chars.peek() {
                    if next.is_ascii_digit() {
                        count = count * 10 + chars.next().unwrap().to_digit(10).unwrap() as u8;
                    }
                }
                // Now get the piece character
                let piece_char = chars.next().ok_or(SfenParseError::InvalidFormat)?;
                Self::add_hand_piece(&mut hands, piece_char, count)?;
            } else {
                Self::add_hand_piece(&mut hands, c, count)?;
            }
        }

        Ok(hands)
    }

    fn add_hand_piece(hands: &mut [Hand; 2], c: char, count: u8) -> Result<(), SfenParseError> {
        let is_black = c.is_ascii_uppercase();
        let color_idx = if is_black { 0 } else { 1 };

        let kind_idx = match c.to_ascii_uppercase() {
            'P' => 0,
            'L' => 1,
            'N' => 2,
            'S' => 3,
            'B' => 4,
            'R' => 5,
            'G' => 6,
            _ => return Err(SfenParseError::InvalidHandPiece(c)),
        };

        hands[color_idx].0[kind_idx] += count;
        Ok(())
    }

    pub fn board(&self) -> [Option<Piece>; 81] {
        let mut board = [None; 81];
        for sq in 0..81 {
            board[sq] = self.at(Square(sq as u8));
        }
        board
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

    pub fn at(&self, square: Square) -> Option<Piece> {
        for (i, bb) in self.piece_bb.iter().enumerate() {
            if bb.at(square) {
                let color = if self.player_bb[0].at(square) {
                    Color::Black
                } else {
                    Color::White
                };
                let kind = PieceKind::from_index(i);
                return Some(Piece::new(color, kind));
            }
        }
        None
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
        let mut king_square = [Square(0); 2];

        for color_index in 0..2 {
            let king_bb = piece_bb[PieceKind::King.index()] & player_bb[color_index];
            if let Some(sq) = king_bb.peek() {
                king_square[color_index] = sq;
            }
        }

        Position {
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
    fn test_from_sfen_startpos() {
        let sfen = "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1";
        let position = Position::from_sfen(sfen).unwrap();
        assert_eq!(position.to_sfen_owned(), sfen);
    }

    #[test]
    fn test_from_sfen_with_hands() {
        let sfen = "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b 2P2p 1";
        let position = Position::from_sfen(sfen).unwrap();
        assert_eq!(position.to_sfen_owned(), sfen);
    }

    #[test]
    fn test_from_sfen_with_promoted() {
        let sfen = "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1+B5R1/LNSGKGSNL w - 10";
        let position = Position::from_sfen(sfen).unwrap();
        assert_eq!(position.to_sfen_owned(), sfen);
    }

    #[test]
    fn test_from_sfen_roundtrip() {
        let original = Position::startpos();
        let sfen = original.to_sfen_owned();
        let parsed = Position::from_sfen(&sfen).unwrap();
        assert_eq!(original, parsed);
    }

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
