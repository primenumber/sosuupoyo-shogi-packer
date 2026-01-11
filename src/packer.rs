use crate::pext::{pext_board_lower_u64x4, pext_u64x4};
use crate::{Color, Hand, PieceKind, Position};

pub const BUFFER_SIZE: usize = 32;

pub trait Packer {
    fn pack(&self, position: &Position, buffer: &mut [u8; BUFFER_SIZE]) -> Option<()>;
    fn unpack(&self, buffer: &[u8; BUFFER_SIZE], position: &mut Position) -> Option<()>;
}

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
            (occupied_lower | (occupied_upper).wrapping_shl(occupied_low_bits)).wrapping_shl(1);
        let occupied_without_kings_compact_upper =
            (occupied_upper).wrapping_shr(63 - occupied_low_bits);

        tmp[0] = occupied_without_kings_compact_lower | position.side_to_move() as u64;
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
}

impl Packer for SSPFv1 {
    fn pack(&self, position: &Position, buf: &mut [u8; BUFFER_SIZE]) -> Option<()> {
        let hand_color_bits =
            Self::pack_hands(&position.hand(Color::Black), &position.hand(Color::White));
        Self::pack_impl(position, hand_color_bits, buf)
    }

    fn unpack(&self, _buffer: &[u8; BUFFER_SIZE], _position: &mut Position) -> Option<()> {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    extern crate test;
    use super::*;
    use crate::position::*;
    use test::Bencher;

    fn init_position() -> Position {
        let board = [
            Some(P_W_LANCE),
            None,
            Some(P_W_PAWN),
            None,
            None,
            None,
            Some(P_B_PAWN),
            None,
            Some(P_B_LANCE),
            Some(P_W_KNIGHT),
            Some(P_W_BISHOP),
            Some(P_W_PAWN),
            None,
            None,
            None,
            Some(P_B_PAWN),
            Some(P_B_ROOK),
            Some(P_B_KNIGHT),
            Some(P_W_SILVER),
            None,
            Some(P_W_PAWN),
            None,
            None,
            None,
            Some(P_B_PAWN),
            None,
            Some(P_B_SILVER),
            Some(P_W_GOLD),
            None,
            Some(P_W_PAWN),
            None,
            None,
            None,
            Some(P_B_PAWN),
            None,
            Some(P_B_GOLD),
            Some(P_W_KING),
            None,
            Some(P_W_PAWN),
            None,
            None,
            None,
            Some(P_B_PAWN),
            None,
            Some(P_B_KING),
            Some(P_W_GOLD),
            None,
            Some(P_W_PAWN),
            None,
            None,
            None,
            Some(P_B_PAWN),
            None,
            Some(P_B_GOLD),
            Some(P_W_SILVER),
            None,
            Some(P_W_PAWN),
            None,
            None,
            None,
            Some(P_B_PAWN),
            None,
            Some(P_B_SILVER),
            Some(P_W_KNIGHT),
            Some(P_W_ROOK),
            Some(P_W_PAWN),
            None,
            None,
            None,
            Some(P_B_PAWN),
            Some(P_B_BISHOP),
            Some(P_B_KNIGHT),
            Some(P_W_LANCE),
            None,
            Some(P_W_PAWN),
            None,
            None,
            None,
            Some(P_B_PAWN),
            None,
            Some(P_B_LANCE),
        ];
        let hands = [Hand([0; 8]), Hand([0; 8])];
        Position::new(board, hands, Color::Black, 0)
    }

    #[test]
    fn test_pack_hands() {
        let hand_black = Hand([2, 1, 0, 0, 1, 0, 0, 0]);
        let hand_white = Hand([1, 0, 1, 0, 0, 1, 0, 0]);
        let packed = SSPFv1::pack_hands(&hand_black, &hand_white);
        assert_eq!(packed, 0b1010100);
    }

    #[test]
    fn test_pack_impl() {
        let position = init_position();

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

    #[bench]
    fn bench_pack(b: &mut Bencher) {
        let position = init_position();
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
}
