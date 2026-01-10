use crate::Bitboard;

#[cfg(target_feature = "bmi2")]
pub fn pext_u64(data: u64, mask: u64) -> u64 {
    use std::arch::x86_64::*;
    unsafe { _pext_u64(data, mask) }
}

#[cfg(not(target_feature = "bmi2"))]
pub fn pext_u64(data: u64, mask: u64) -> u64 {
    let mut result = 0;
    let mut pos_bit = 1u64;
    for i in 0..64 {
        if (mask >> i) & 1 != 0 {
            if (data >> i) & 1 != 0 {
                result |= pos_bit;
            }
            pos_bit = pos_bit.wrapping_shl(1);
        }
    }
    result
}

pub fn pext_board_as_u128(data: Bitboard, mask: Bitboard) -> u128 {
    let bits0 = pext_u64(data.0, mask.0);
    let bits1 = pext_u64(data.1, mask.1);
    let shift = mask.0.count_ones();
    let result0 = bits0 | bits1.wrapping_shl(shift);
    let result1 = bits1.wrapping_shr(64 - shift);
    (result1 as u128).wrapping_shl(64) | (result0 as u128)
}

pub fn pext_board_lower_u64(data: Bitboard, mask: Bitboard) -> u64 {
    let bits0 = pext_u64(data.0, mask.0);
    let bits1 = pext_u64(data.1, mask.1);
    let shift = mask.0.count_ones();
    bits0 | bits1.wrapping_shl(shift)
}
