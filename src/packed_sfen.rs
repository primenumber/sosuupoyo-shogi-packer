//! YaneuraOu PackedSfen format implementation.
//!
//! PackedSfen is a 256-bit (32-byte) compact representation of a shogi position
//! used by YaneuraOu and related shogi engines for NNUE training data.
//!
//! ## Encoding Format
//!
//! The encoding uses Huffman coding for board pieces (excluding kings):
//! - Empty: 0 (1 bit)
//! - Pawn: 10 + color(1) + promoted(1) = 4 bits
//! - Lance: 1100 + color(1) + promoted(1) = 6 bits
//! - Knight: 1101 + color(1) + promoted(1) = 6 bits
//! - Silver: 1110 + color(1) + promoted(1) = 6 bits
//! - Gold: 11110 + color(1) = 6 bits
//! - Bishop: 111110 + color(1) + promoted(1) = 8 bits
//! - Rook: 111111 + color(1) + promoted(1) = 8 bits
//!
//! Kings are stored separately as 7-bit square indices (2 × 7 = 14 bits).
//! King squares are skipped in the board encoding (not even marked as empty).
//!
//! Hand pieces use compact Huffman encoding (no empty branch, no promoted bit):
//! - Pawn: 0 + color(1) = 2 bits
//! - Lance: 100 + color(1) = 4 bits
//! - Knight: 101 + color(1) = 4 bits
//! - Silver: 110 + color(1) = 4 bits
//! - Gold: 1110 + color(1) = 5 bits
//! - Bishop: 11110 + color(1) = 6 bits
//! - Rook: 11111 + color(1) = 6 bits
//! Terminator: all remaining bits are 0 (no more pieces when all-zero pattern detected)

use crate::packer::{Packer, BUFFER_SIZE};
use crate::{Color, Hand, OptionPiece, Piece, PieceKind, Position, Square};

pub struct PackedSfen;

/// Bit stream writer for packing
struct BitWriter<'a> {
    buffer: &'a mut [u64],
    cursor: u32, // bit position
}

impl<'a> BitWriter<'a> {
    fn new(buffer: &'a mut [u64]) -> Self {
        buffer.fill(0);
        Self { buffer, cursor: 0 }
    }

    fn write_n_bit(&mut self, mut value: u64, mut num_bits: u32) {
        while num_bits > 0 {
            let word_pos = self.cursor >> 6;
            let bit_pos = self.cursor & 63;
            unsafe { *self.buffer.get_unchecked_mut(word_pos as usize) |= value << bit_pos };
            let bits_in_current_word = 64 - bit_pos;
            let bits_written = bits_in_current_word.min(num_bits);
            self.cursor += bits_written;
            num_bits -= bits_written;
            value >>= bits_written;
        }
    }

    fn write_one_bit(&mut self, value: u64) {
        if value != 0 {
            let word_pos = self.cursor >> 6;
            let bit_pos = self.cursor & 63;
            unsafe { *self.buffer.get_unchecked_mut(word_pos as usize) |= 1 << bit_pos };
        }
        self.cursor += 1;
    }
}

/// Bit stream reader for unpacking
struct BitReader<'a> {
    buffer: &'a [u64],
    cursor: u32, // bit position
}

impl<'a> BitReader<'a> {
    fn new(buffer: &'a [u64]) -> Self {
        Self { buffer, cursor: 0 }
    }

    fn read_n_bit(&mut self, num_bits: u32) -> u64 {
        let mut result = 0u64;
        for i in 0..num_bits {
            result |= if self.read_one_bit() != 0 { 1 << i } else { 0 };
        }
        result
    }

    fn read_one_bit(&mut self) -> u64 {
        let word_pos = self.cursor >> 6;
        let bit_pos = self.cursor & 63;
        let bit =
            ((unsafe { *self.buffer.get_unchecked(word_pos as usize) } >> bit_pos) & 1) as u64;
        self.cursor += 1;
        bit
    }
}

const HUFFMAN_TABLE: [(u64, u32); 8] = [
    // (bit pattern as u32, number of bits)
    (0b0, 1),      // Empty
    (0b01, 2),     // Pawn
    (0b0011, 4),   // Lance
    (0b1011, 4),   // Knight
    (0b0111, 4),   // Silver
    (0b011111, 6), // Bishop
    (0b111111, 6), // Rook
    (0b01111, 5),  // Gold
];

impl PackedSfen {
    /// Write a piece (excluding King) to the bit stream using Huffman encoding.
    fn write_piece(writer: &mut BitWriter, piece: OptionPiece) {
        match piece.to_option() {
            None => {
                writer.write_one_bit(0); // Empty: 0
            }
            Some(p) => {
                let color_bit = p.color() as u64;
                let kind = p.kind();
                let is_promoted = kind.index() >= PieceKind::ProPawn.index();

                let raw_kind_index = kind.index() & 0x7; // Remove promoted flag
                let (bits, length) = unsafe { *HUFFMAN_TABLE.get_unchecked(raw_kind_index + 1) };
                // +1 to skip empty
                writer.write_n_bit(bits, length);
                writer.write_one_bit(color_bit);
                if kind == PieceKind::Gold {
                    return; // Gold has no promoted bit
                }
                writer.write_one_bit(if is_promoted { 1 } else { 0 });
            }
        }
    }

    /// Write a hand piece to the bit stream using compact Huffman encoding.
    /// Compact encoding removes the "empty square" branch, saving 1 bit per piece.
    fn write_hand_piece(writer: &mut BitWriter, kind: PieceKind, color: Color) {
        let color_bit = color as u64;
        let kind_index = kind.index();
        let (bits, length) = unsafe { *HUFFMAN_TABLE.get_unchecked(kind_index + 1) };
        writer.write_n_bit(bits >> 1, length - 1); // -1 to remove empty branch
        writer.write_one_bit(color_bit);
        if kind == PieceKind::Gold {
            return; // Gold has no promoted bit
        }
        writer.write_one_bit(0); // all hand pieces are unpromoted
    }

    /// Read a hand piece from the bit stream using compact Huffman encoding.
    /// Returns None if no more pieces can be read.
    fn read_hand_piece(reader: &mut BitReader) -> Option<(PieceKind, Color)> {
        // Need at least 2 bits to read a piece (pawn = 0 + color)
        if reader.cursor + 2 > 256 {
            return None;
        }

        // 0 = pawn
        if reader.read_one_bit() == 0 {
            let color = Color::from_index(reader.read_one_bit() as usize);
            reader.read_one_bit(); // promoted bit (always 0 for hand pieces)
            return Some((PieceKind::Pawn, color));
        }

        // Need at least 3 more bits (10x + color)
        if reader.cursor + 3 > 256 {
            return None;
        }

        // 10x = lance/knight
        if reader.read_one_bit() == 0 {
            if reader.read_one_bit() == 0 {
                // 100 = lance
                let color = Color::from_index(reader.read_one_bit() as usize);
                reader.read_one_bit(); // promoted bit (always 0 for hand pieces)
                return Some((PieceKind::Lance, color));
            } else {
                // 101 = knight
                let color = Color::from_index(reader.read_one_bit() as usize);
                reader.read_one_bit(); // promoted bit (always 0 for hand pieces)
                return Some((PieceKind::Knight, color));
            }
        }

        // 110 = silver
        if reader.read_one_bit() == 0 {
            let color = Color::from_index(reader.read_one_bit() as usize);
            reader.read_one_bit(); // promoted bit (always 0 for hand pieces)
            return Some((PieceKind::Silver, color));
        }

        // Need at least 2 more bits
        if reader.cursor + 2 > 256 {
            return None;
        }

        // 1110 = gold
        if reader.read_one_bit() == 0 {
            let color = Color::from_index(reader.read_one_bit() as usize);
            return Some((PieceKind::Gold, color));
        }

        // Need at least 2 more bits
        if reader.cursor + 2 > 256 {
            return None;
        }

        // 11110 = bishop, 11111 = rook
        if reader.read_one_bit() == 0 {
            let color = Color::from_index(reader.read_one_bit() as usize);
            reader.read_one_bit(); // promoted bit (always 0 for hand pieces)
            return Some((PieceKind::Bishop, color));
        }

        // 11111 = rook
        let color = Color::from_index(reader.read_one_bit() as usize);
        reader.read_one_bit(); // promoted bit (always 0 for hand pieces)
        Some((PieceKind::Rook, color))
    }

    /// Read a piece (excluding King) from the bit stream.
    fn read_piece(reader: &mut BitReader) -> OptionPiece {
        // 0 = empty
        if reader.read_one_bit() == 0 {
            return OptionPiece::none();
        }

        // 10 = pawn
        if reader.read_one_bit() == 0 {
            let color = Color::from_index(reader.read_one_bit() as usize);
            let promoted = reader.read_one_bit() == 1;
            let kind = if promoted {
                PieceKind::ProPawn
            } else {
                PieceKind::Pawn
            };
            return OptionPiece::some(Piece::new(color, kind));
        }

        // 110x = lance/knight
        if reader.read_one_bit() == 0 {
            if reader.read_one_bit() == 0 {
                // 1100 = lance
                let color = Color::from_index(reader.read_one_bit() as usize);
                let promoted = reader.read_one_bit() == 1;
                let kind = if promoted {
                    PieceKind::ProLance
                } else {
                    PieceKind::Lance
                };
                return OptionPiece::some(Piece::new(color, kind));
            } else {
                // 1101 = knight
                let color = Color::from_index(reader.read_one_bit() as usize);
                let promoted = reader.read_one_bit() == 1;
                let kind = if promoted {
                    PieceKind::ProKnight
                } else {
                    PieceKind::Knight
                };
                return OptionPiece::some(Piece::new(color, kind));
            }
        }

        // 1110 = silver
        if reader.read_one_bit() == 0 {
            let color = Color::from_index(reader.read_one_bit() as usize);
            let promoted = reader.read_one_bit() == 1;
            let kind = if promoted {
                PieceKind::ProSilver
            } else {
                PieceKind::Silver
            };
            return OptionPiece::some(Piece::new(color, kind));
        }

        // 11110 = gold
        if reader.read_one_bit() == 0 {
            let color = Color::from_index(reader.read_one_bit() as usize);
            return OptionPiece::some(Piece::new(color, PieceKind::Gold));
        }

        // 111110 = bishop, 111111 = rook
        if reader.read_one_bit() == 0 {
            // Bishop
            let color = Color::from_index(reader.read_one_bit() as usize);
            let promoted = reader.read_one_bit() == 1;
            let kind = if promoted {
                PieceKind::ProBishop
            } else {
                PieceKind::Bishop
            };
            return OptionPiece::some(Piece::new(color, kind));
        }

        // 111111 = rook
        let color = Color::from_index(reader.read_one_bit() as usize);
        let promoted = reader.read_one_bit() == 1;
        let kind = if promoted {
            PieceKind::ProRook
        } else {
            PieceKind::Rook
        };
        OptionPiece::some(Piece::new(color, kind))
    }
}

impl Packer for PackedSfen {
    fn pack(&self, position: &Position, buffer: &mut [u8; BUFFER_SIZE]) -> Option<()> {
        let mut tmp = [0u64; 4];
        let mut writer = BitWriter::new(&mut tmp);

        // Side to move (1 bit)
        writer.write_one_bit(position.side_to_move() as u64);

        // Get king positions
        let black_king_sq = position.king_square(Color::Black).0 as usize;
        let white_king_sq = position.king_square(Color::White).0 as usize;

        // First, encode king positions (7 bits each = 14 bits total)
        writer.write_n_bit(black_king_sq as u64, 7);
        writer.write_n_bit(white_king_sq as u64, 7);

        // Encode board (79 squares, skipping king squares)
        for sq in 0..81 {
            // Skip king squares
            if sq == black_king_sq || sq == white_king_sq {
                continue;
            }
            Self::write_piece(&mut writer, position.at(Square(sq as u8)));
        }

        // Hand pieces: Huffman encoded
        // Order: Rook, Bishop, Gold, Silver, Knight, Lance, Pawn (for each color)
        let hand_piece_kinds = [
            PieceKind::Pawn,
            PieceKind::Lance,
            PieceKind::Knight,
            PieceKind::Silver,
            PieceKind::Gold,
            PieceKind::Bishop,
            PieceKind::Rook,
        ];

        for color in [Color::Black, Color::White] {
            let hand = position.hand(color);
            for &kind in &hand_piece_kinds {
                let count = hand.0[kind.index()];
                for _ in 0..count {
                    Self::write_hand_piece(&mut writer, kind, color);
                }
            }
        }

        // Copy to output buffer
        for (i, word) in tmp.iter().enumerate() {
            buffer[(i * 8)..(i * 8 + 8)].copy_from_slice(&word.to_le_bytes());
        }

        Some(())
    }

    fn unpack(&self, buffer: &[u8; BUFFER_SIZE], position: &mut Position) -> Option<()> {
        let mut tmp = [0u64; 4];
        for i in 0..4 {
            tmp[i] = u64::from_le_bytes(buffer[(i * 8)..(i * 8 + 8)].try_into().unwrap());
        }
        let mut reader = BitReader::new(&tmp);

        // Side to move
        let side_to_move = Color::from_index(reader.read_one_bit() as usize);

        // Read king positions (7 bits each)
        let black_king_sq = reader.read_n_bit(7) as usize;
        let white_king_sq = reader.read_n_bit(7) as usize;

        // Decode board (79 squares, skipping king squares)
        let mut board = [OptionPiece::none(); 81];
        for sq in 0..81 {
            if sq == black_king_sq || sq == white_king_sq {
                continue;
            }
            board[sq] = Self::read_piece(&mut reader);
        }

        // Place kings
        board[black_king_sq] = OptionPiece::some(Piece::new(Color::Black, PieceKind::King));
        board[white_king_sq] = OptionPiece::some(Piece::new(Color::White, PieceKind::King));

        // Count pieces on board to determine how many hand pieces to read
        // PieceKind order: Pawn=0, Lance=1, Knight=2, Silver=3, Bishop=4, Rook=5, Gold=6, King=7
        let mut board_piece_counts = [0u8; 8];
        for sq in 0..81 {
            if let Some(piece) = board[sq].to_option() {
                let base_idx = piece.kind().index() & 0x7; // Remove promoted flag
                board_piece_counts[base_idx] += 1;
            }
        }

        // Maximum pieces for each type (indexed by PieceKind)
        // Pawn=18, Lance=4, Knight=4, Silver=4, Bishop=2, Rook=2, Gold=4
        let max_counts: [u8; 8] = [18, 4, 4, 4, 2, 2, 4, 0]; // Last is King (not in hand)

        // Read hand pieces (Huffman encoded)
        let mut black_hand = Hand([0; 8]);
        let mut white_hand = Hand([0; 8]);

        // Calculate how many hand pieces we need to read for each type
        let mut remaining_counts = [0u8; 8];
        for i in 0..7 {
            remaining_counts[i] = max_counts[i].saturating_sub(board_piece_counts[i]);
        }

        // Read hand pieces until all expected pieces are read
        let total_hand_pieces: u8 = remaining_counts.iter().sum();
        let mut read_count = 0u8;

        while read_count < total_hand_pieces && reader.cursor < 256 {
            if let Some((kind, color)) = Self::read_hand_piece(&mut reader) {
                let idx = kind.index();
                // Validate piece count
                if remaining_counts[idx] == 0 {
                    break; // No more of this piece type expected
                }
                remaining_counts[idx] -= 1;
                read_count += 1;

                match color {
                    Color::Black => black_hand.0[idx] += 1,
                    Color::White => white_hand.0[idx] += 1,
                }
            } else {
                break;
            }
        }

        let hands = [black_hand, white_hand];

        // Reconstruct position
        *position = Position::new(board, hands, side_to_move, 1);

        Some(())
    }
}

#[cfg(test)]
mod tests {
    extern crate test;
    use super::*;
    use crate::packer::BUFFER_SIZE;
    use crate::position::*;
    use std::hint::black_box;
    use test::Bencher;

    #[test]
    fn test_pack_unpack_roundtrip() {
        let sfens = [
            "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1",
            "3k3nl/G3n2r1/P1P+LB+B1p1/1G2ppp1p/KP1P1P1P1/p2sP1P1P/5SN2/4G4/L+R6L w 2Ppn2SG 1",
        ];
        for sfen in sfens.iter() {
            let position = Position::from_sfen(sfen).unwrap();
            let mut buffer = [0u8; BUFFER_SIZE];

            PackedSfen.pack(&position, &mut buffer).unwrap();

            let mut unpacked = Position::startpos();
            PackedSfen.unpack(&buffer, &mut unpacked).unwrap();

            assert_eq!(unpacked, position);
        }
    }

    #[test]
    fn test_pack_unpack_with_empty_squares() {
        // Test with removed pieces (no hands - hands are computed from board pieces)
        let mut board = Position::startpos().board();
        // Remove some pieces from board to create empty squares
        board[0] = OptionPiece::none(); // Remove lance
        board[72] = OptionPiece::none(); // Remove lance

        // Hands are zeros since we don't have enough bits to encode them
        // in the current format when all pieces are on board
        let hands = [Hand([0; 8]), Hand([0; 8])];
        let position = Position::new(board, hands, Color::Black, 1);

        let mut buffer = [0u8; BUFFER_SIZE];
        PackedSfen.pack(&position, &mut buffer).unwrap();

        let mut unpacked = Position::startpos();
        PackedSfen.unpack(&buffer, &mut unpacked).unwrap();

        assert_eq!(unpacked, position);
    }

    #[test]
    fn test_bit_writer_reader_roundtrip() {
        let mut buffer = [0u64; 4];
        {
            let mut writer = BitWriter::new(&mut buffer);
            writer.write_n_bit(0b101, 3);
            writer.write_n_bit(0b1100, 4);
            writer.write_n_bit(0b11111111, 8);
            writer.write_n_bit(0b0, 1);
        }

        let mut reader = BitReader::new(&buffer);
        assert_eq!(reader.read_n_bit(3), 0b101);
        assert_eq!(reader.read_n_bit(4), 0b1100);
        assert_eq!(reader.read_n_bit(8), 0b11111111);
        assert_eq!(reader.read_n_bit(1), 0b0);
    }

    #[test]
    fn test_encode_decode_empty() {
        let mut buffer = [0u64; 1];
        {
            let mut writer = BitWriter::new(&mut buffer);
            PackedSfen::write_piece(&mut writer, OptionPiece::none());
        }
        let mut reader = BitReader::new(&buffer);
        let decoded = PackedSfen::read_piece(&mut reader);
        assert_eq!(decoded, OptionPiece::none());
    }

    #[test]
    fn test_encode_decode_piece_roundtrip() {
        for kind in 0..14 {
            let kind = PieceKind::from_index(kind);
            if kind == PieceKind::King {
                continue; // Skip king
            }
            for color in 0..2 {
                let color = Color::from_index(color);
                let piece = OptionPiece::some(Piece::new(color, kind));
                let mut buffer = [0u64; 1];
                {
                    let mut writer = BitWriter::new(&mut buffer);
                    PackedSfen::write_piece(&mut writer, piece);
                }
                let mut reader = BitReader::new(&buffer);
                let decoded = PackedSfen::read_piece(&mut reader);
                assert_eq!(decoded, piece, "Failed for piece {:?}", piece);
            }
        }
    }

    #[bench]
    fn bench_pack(b: &mut Bencher) {
        let position = Position::startpos();
        let mut buffer = [0u8; BUFFER_SIZE];
        b.iter(|| {
            PackedSfen
                .pack(black_box(&position), black_box(&mut buffer))
                .unwrap();
        });
    }

    #[bench]
    fn bench_unpack(b: &mut Bencher) {
        let position = Position::startpos();
        let mut buffer = [0u8; BUFFER_SIZE];
        PackedSfen.pack(&position, &mut buffer).unwrap();

        let mut unpacked = Position::startpos();
        b.iter(|| {
            PackedSfen
                .unpack(black_box(&buffer), black_box(&mut unpacked))
                .unwrap();
        });
    }
}
