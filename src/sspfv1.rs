use crate::packer::{Packer, BUFFER_SIZE};
use crate::pdep::{pdep_board, pdep_board_lower_u64};
use crate::pext::{pext_board_lower_u64x4, pext_u64x4};
use crate::{Bitboard, Color, Hand, PieceKind, Position, Square};

pub struct SSPFv1();

impl SSPFv1 {
    fn pack_hands(hand_black: &Hand, hand_white: &Hand) -> u64 {
        let mut result = 0u64;
        let mut count = 0;
        let hand_order = [
            PieceKind::Pawn,
            PieceKind::Lance,
            PieceKind::Knight,
            PieceKind::Silver,
            PieceKind::Gold,
            PieceKind::Bishop,
            PieceKind::Rook,
        ];
        for pt in hand_order.iter() {
            let i = pt.index();
            let count_black = hand_black.0[i] as u32;
            let count_white = hand_white.0[i] as u32;
            count += count_black;
            result |= (1u64.wrapping_shl(count_white) - 1).wrapping_shl(count);
            count += count_white;
        }
        result
    }

    // Pack without hands and color bits
    // Assumes that hand_color_bits is already prepared
    pub fn pack_impl(
        position: &Position,
        hand_color_bits: u64,
        buf: &mut [u8; BUFFER_SIZE],
    ) -> Option<()> {
        let mut tmp = [0u64; 4];
        let black_bb = position.player_bb(Color::Black);
        let white_bb = position.player_bb(Color::White);
        let occupied = black_bb | white_bb;
        let king_bb = position.piece_kind_bitboard(PieceKind::King);
        let occupied_without_kings_bb = occupied ^ king_bb;
        let black_king_pos = position.king_square(Color::Black).0 as u64;
        let white_king_pos = position.king_square(Color::White).0 as u64;

        let pawn_bb = position.piece_kind_bitboard(PieceKind::Pawn)
            | position.piece_kind_bitboard(PieceKind::ProPawn);
        let non_pawn_pieces_bb = occupied_without_kings_bb ^ pawn_bb;
        // Notice: lance does not need to be calculated explicitly
        let knight_bb = position.piece_kind_bitboard(PieceKind::Knight)
            | position.piece_kind_bitboard(PieceKind::ProKnight);
        let silver_bb = position.piece_kind_bitboard(PieceKind::Silver)
            | position.piece_kind_bitboard(PieceKind::ProSilver);
        let bishop_bb = position.piece_kind_bitboard(PieceKind::Bishop)
            | position.piece_kind_bitboard(PieceKind::ProBishop);
        let rook_bb = position.piece_kind_bitboard(PieceKind::Rook)
            | position.piece_kind_bitboard(PieceKind::ProRook);
        let gold_bb = position.piece_kind_bitboard(PieceKind::Gold);
        let promoted_bb = position.piece_kind_bitboard(PieceKind::ProPawn)
            | position.piece_kind_bitboard(PieceKind::ProLance)
            | position.piece_kind_bitboard(PieceKind::ProKnight)
            | position.piece_kind_bitboard(PieceKind::ProSilver)
            | position.piece_kind_bitboard(PieceKind::ProBishop)
            | position.piece_kind_bitboard(PieceKind::ProRook);

        let [knight_bits, silver_bits, bishop_bits, rook_bits] = pext_board_lower_u64x4(
            [knight_bb, silver_bb, bishop_bb, rook_bb],
            [
                non_pawn_pieces_bb,
                non_pawn_pieces_bb,
                non_pawn_pieces_bb,
                non_pawn_pieces_bb,
            ],
        );
        let [non_pawn_bits, color_board_bits, gold_bits, promoted_bits] = pext_board_lower_u64x4(
            [non_pawn_pieces_bb, white_bb, gold_bb, promoted_bb],
            [
                occupied_without_kings_bb,
                occupied_without_kings_bb,
                non_pawn_pieces_bb,
                occupied_without_kings_bb ^ gold_bb,
            ],
        );
        let hand_bits_offset =
            occupied_without_kings_bb.0.count_ones() + occupied_without_kings_bb.1.count_ones();
        let color_bits = color_board_bits | hand_color_bits.wrapping_shl(hand_bits_offset);

        let bit3_mask = bishop_bits | rook_bits | gold_bits;
        let bit1 = silver_bits | bit3_mask;
        let bit2 = knight_bits | bit3_mask;
        let [occupied_lower, occupied_upper, bit3, bit4] = pext_u64x4(
            [occupied.0, occupied.1, bishop_bits | rook_bits, rook_bits],
            [!king_bb.0, !king_bb.1, bit3_mask, bishop_bits | rook_bits],
        );
        let occupied_low_bits = (!king_bb.0 & 0x7FFF_FFFF_FFFF_FFFF).count_ones();
        let occupied_without_kings_compact_lower =
            occupied_lower | (occupied_upper).wrapping_shl(occupied_low_bits);
        let occupied_without_kings_compact_upper =
            (occupied_upper).wrapping_shr(63 - occupied_low_bits);

        tmp[0] =
            occupied_without_kings_compact_lower.wrapping_shl(1) | position.side_to_move() as u64;
        tmp[1] = occupied_without_kings_compact_upper
            | black_king_pos.wrapping_shl(16)
            | white_king_pos.wrapping_shl(23)
            | promoted_bits.wrapping_shl(30);
        tmp[2] = non_pawn_bits | bit1.wrapping_shl(38) | bit2.wrapping_shl(58);
        tmp[3] = bit2.wrapping_shr(6)
            | bit3.wrapping_shl(14)
            | bit4.wrapping_shl(22)
            | color_bits.wrapping_shl(26);
        for i in 0..4 {
            buf[i * 8..(i + 1) * 8].copy_from_slice(&tmp[i].to_le_bytes());
        }
        Some(())
    }

    fn unpack_hands(hand_color_bits: u64, counts: [u8; 8]) -> (Hand, Hand) {
        let mut hand_black = Hand([0; 8]);
        let mut hand_white = Hand([0; 8]);
        let hand_order = [
            PieceKind::Pawn,
            PieceKind::Lance,
            PieceKind::Knight,
            PieceKind::Silver,
            PieceKind::Gold,
            PieceKind::Bishop,
            PieceKind::Rook,
        ];
        let mut count = 0;
        for pt in hand_order.iter() {
            let i = pt.index();
            let total_count = counts[i] as u32;
            let mut count_white = 0u32;
            for _ in 0..total_count {
                if (hand_color_bits >> count) & 1 == 1 {
                    count_white += 1;
                }
                count += 1;
            }
            let count_black = total_count - count_white;
            hand_black.0[i] = count_black as u8;
            hand_white.0[i] = count_white as u8;
        }
        (hand_black, hand_white)
    }
}

impl Packer for SSPFv1 {
    fn pack(&self, position: &Position, buf: &mut [u8; BUFFER_SIZE]) -> Option<()> {
        let hand_color_bits =
            Self::pack_hands(&position.hand(Color::Black), &position.hand(Color::White));
        Self::pack_impl(position, hand_color_bits, buf)
    }

    fn unpack(&self, buffer: &[u8; BUFFER_SIZE], position: &mut Position) -> Option<()> {
        let mut tmp = [0u64; 4];
        let (chunks, _) = buffer.as_chunks::<8>();
        for (i, &chunk) in chunks.iter().enumerate() {
            tmp[i] = u64::from_le_bytes(chunk);
        }

        let side_to_move = tmp[0] & 1;
        let occupied_without_kings_compact_lower = tmp[0] >> 1;
        let occupied_without_kings_compact_upper = tmp[1] & 0xFFFF;
        let black_king_pos = Square(((tmp[1] >> 16) & 0x7F) as u8);
        let white_king_pos = Square(((tmp[1] >> 23) & 0x7F) as u8);
        let promoted_bits = tmp[1] >> 30;
        let non_pawn_bits = tmp[2] & 0x3F_FFFF_FFFF;
        let bit1 = (tmp[2] >> 38) & 0xF_FFFF;
        let bit2_lower = (tmp[2] >> 58) & 0x3F;
        let bit2_upper = tmp[3] & 0x3FFF;
        let bit2 = bit2_lower | (bit2_upper << 6);
        let bit3 = (tmp[3] >> 14) & 0xFF;
        let bit4 = (tmp[3] >> 22) & 0xF;
        let color_bits = tmp[3] >> 26;
        let num_occupied_without_kings = occupied_without_kings_compact_lower.count_ones()
            + occupied_without_kings_compact_upper.count_ones();
        let hand_counts = [
            18 + non_pawn_bits.count_ones() as u8 - num_occupied_without_kings as u8, // pawn
            4 + (bit1 | bit2).count_ones() as u8 - non_pawn_bits.count_ones() as u8,  // lance
            4 - (bit2 & !bit1).count_ones() as u8,                                    // knight
            4 - (bit1 & !bit2).count_ones() as u8,                                    // silver
            2 - bit4.count_ones() as u8,                                              // rook
            2 + bit4.count_ones() as u8 - bit3.count_ones() as u8,                    // bishop
            4 + bit3.count_ones() as u8 - (bit1 & bit2).count_ones() as u8,           // gold
            0,                                                                        // padding
        ];

        let occupied_without_kings_bb = Bitboard(
            occupied_without_kings_compact_lower,
            occupied_without_kings_compact_upper,
        );
        let kings_bb =
            Bitboard::from_square(black_king_pos) | Bitboard::from_square(white_king_pos);
        let occupied_without_kings_bb = pdep_board(occupied_without_kings_bb, !kings_bb);
        let occupied_bb = occupied_without_kings_bb | kings_bb;
        let non_pawn_bb = pdep_board_lower_u64(non_pawn_bits, occupied_without_kings_bb);
        let pawn_bb = occupied_without_kings_bb ^ non_pawn_bb;
        let bit1_bb = pdep_board_lower_u64(bit1, non_pawn_bb);
        let bit2_bb = pdep_board_lower_u64(bit2, non_pawn_bb);
        let lance_bb = non_pawn_bb ^ (bit1_bb | bit2_bb);
        let knight_bb = bit2_bb & !bit1_bb;
        let silver_bb = bit1_bb & !bit2_bb;
        let bishop_rook_gold_bb = bit1_bb & bit2_bb;
        let bit3_bb = pdep_board_lower_u64(bit3, bishop_rook_gold_bb);
        let bit4_bb = pdep_board_lower_u64(bit4, bit3_bb);
        let gold_bb = bishop_rook_gold_bb ^ bit3_bb;
        let rook_bb = bit4_bb;
        let bishop_bb = bit3_bb ^ bit4_bb;
        let promoted_bb = pdep_board_lower_u64(promoted_bits, occupied_without_kings_bb ^ gold_bb);
        let hand_bits_offset = occupied_without_kings_compact_lower.count_ones()
            + occupied_without_kings_compact_upper.count_ones();
        let hand_color_bits = color_bits >> hand_bits_offset;
        let board_color_bits = color_bits ^ (hand_color_bits << hand_bits_offset);
        let color_bb = pdep_board_lower_u64(board_color_bits, occupied_without_kings_bb)
            | Bitboard::from_square(white_king_pos);
        // Reconstruct position
        let piece_bb = [
            pawn_bb & !promoted_bb,
            lance_bb & !promoted_bb,
            knight_bb & !promoted_bb,
            silver_bb & !promoted_bb,
            bishop_bb & !promoted_bb,
            rook_bb & !promoted_bb,
            gold_bb,
            kings_bb,
            pawn_bb & promoted_bb,
            lance_bb & promoted_bb,
            knight_bb & promoted_bb,
            silver_bb & promoted_bb,
            bishop_bb & promoted_bb,
            rook_bb & promoted_bb,
        ];
        let player_bb = [occupied_bb ^ color_bb, color_bb];
        let (hand_black, hand_white) = Self::unpack_hands(hand_color_bits, hand_counts);
        let hands = [hand_black, hand_white];

        *position = Position::from_bitboards(
            player_bb,
            piece_bb,
            hands,
            Color::from_index(side_to_move as usize),
            1,
        );
        Some(())
    }
}

#[cfg(test)]
mod tests {
    extern crate test;
    use super::*;
    use crate::position::*;
    use test::Bencher;

    #[test]
    fn test_pack_hands() {
        let hand_black = Hand([2, 1, 0, 0, 1, 0, 0, 0]);
        let hand_white = Hand([1, 0, 1, 0, 0, 1, 0, 0]);
        let packed = SSPFv1::pack_hands(&hand_black, &hand_white);
        assert_eq!(packed, 0b1010100);
    }

    #[test]
    fn test_pack_impl() {
        let position = Position::startpos();

        let mut buffer = [0u8; BUFFER_SIZE];
        SSPFv1::pack_impl(&position, 0, &mut buffer).unwrap();

        assert_eq!(
            buffer,
            [
                0x8A, 0x1E, 0x2F, 0x5A, 0x54, 0x54, 0xB4, 0xE8, 0xF1, 0xA2, 0x2C, 0x12, 0x00, 0x00,
                0x00, 0x00, 0x39, 0x67, 0x92, 0x39, 0x27, 0xF6, 0x6F, 0xF0, 0x3C, 0xCF, 0xB0, 0xCD,
                0x31, 0xD3, 0xCC, 0x31
            ]
        );
    }

    #[test]
    fn test_pack_unpack_roundtrip() {
        let position = Position::startpos();
        let mut buffer = [0u8; BUFFER_SIZE];

        SSPFv1().pack(&position, &mut buffer).unwrap();

        let mut unpacked = Position::startpos();
        SSPFv1().unpack(&buffer, &mut unpacked).unwrap();

        assert_eq!(unpacked, position);
    }

    #[test]
    fn test_pack_unpack_roundtrip_with_hands() {
        let position = Position::startpos();
        let mut board = position.board().clone();
        board[10] = None; // Remove White's Bishop from the board
        board[16] = None; // Remove Black's Rook from the board
        board[64] = None; // Remove White's Rook from the board
        board[70] = None; // Remove Black's Bishop from the board

        // Black has: Bishop, Rook
        // White has: Bishop, Rook
        let hands = [
            Hand([0, 0, 0, 0, 1, 1, 0, 0]), // Black: 1 Bishop, 1 Rook
            Hand([0, 0, 0, 0, 1, 1, 0, 0]), // White: 1 Bishop, 1 Rook
        ];
        let position = Position::new(board, hands, Color::Black, 1);

        let mut buffer = [0u8; BUFFER_SIZE];
        SSPFv1().pack(&position, &mut buffer).unwrap();

        let mut unpacked = Position::set_only_kings();
        SSPFv1().unpack(&buffer, &mut unpacked).unwrap();

        // Verify side_to_move
        assert_eq!(
            unpacked.side_to_move() as u8,
            position.side_to_move() as u8,
            "side_to_move mismatch"
        );

        // Verify hands
        assert_eq!(
            unpacked.hand(Color::Black).0,
            position.hand(Color::Black).0,
            "black hand mismatch"
        );
        assert_eq!(
            unpacked.hand(Color::White).0,
            position.hand(Color::White).0,
            "white hand mismatch"
        );

        // Verify piece bitboards
        for kind in [
            PieceKind::Pawn,
            PieceKind::Lance,
            PieceKind::Knight,
            PieceKind::Silver,
            PieceKind::Bishop,
            PieceKind::Rook,
            PieceKind::Gold,
            PieceKind::King,
        ] {
            assert_eq!(
                unpacked.piece_kind_bitboard(kind),
                position.piece_kind_bitboard(kind),
                "{:?} bitboard mismatch",
                kind
            );
        }
    }

    #[bench]
    fn bench_pack(b: &mut Bencher) {
        let position = Position::startpos();
        let mut buffer = [0u8; BUFFER_SIZE];
        let mut sum = 0;
        b.iter(|| {
            SSPFv1().pack(&position, &mut buffer).unwrap();
            // Prevent optimization
            for byte in buffer.iter() {
                sum += *byte as usize;
            }
        });
        assert_ne!(sum, 0);
    }

    #[bench]
    fn bench_unpack(b: &mut Bencher) {
        let position = Position::startpos();
        let mut buffer = [0u8; BUFFER_SIZE];
        SSPFv1().pack(&position, &mut buffer).unwrap();

        let mut unpacked = Position::set_only_kings();
        let mut sum = 0;
        b.iter(|| {
            SSPFv1().unpack(&buffer, &mut unpacked).unwrap();
            // Prevent optimization
            sum += unpacked.king_square(Color::Black).0 as usize;
        });
        assert_ne!(sum, 0);
    }
}
