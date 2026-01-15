use crate::Bitboard;

#[cfg(target_feature = "bmi2")]
#[inline(always)]
pub fn pext_u64(data: u64, mask: u64) -> u64 {
    use std::arch::x86_64::*;
    unsafe { _pext_u64(data, mask) }
}

#[cfg(not(target_feature = "bmi2"))]
#[inline(always)]
pub fn pext_u64(data: u64, mut mask: u64) -> u64 {
    let mut result = 0;
    let mut pos_bit = 1u64;
    while mask != 0 {
        let lsb_bit = mask & mask.wrapping_neg();
        if data & lsb_bit != 0 {
            result |= pos_bit;
        }
        pos_bit = pos_bit.wrapping_shl(1);
        mask ^= lsb_bit;
    }
    result
}

#[cfg(any(target_feature = "bmi2", not(target_feature = "avx2")))]
#[inline(always)]
pub fn pext_u64x4(data: [u64; 4], mask: [u64; 4]) -> [u64; 4] {
    [
        pext_u64(data[0], mask[0]),
        pext_u64(data[1], mask[1]),
        pext_u64(data[2], mask[2]),
        pext_u64(data[3], mask[3]),
    ]
}

#[cfg(all(not(target_feature = "bmi2"), target_feature = "avx2"))]
#[inline(always)]
pub fn pext_u64x4(data: [u64; 4], mask: [u64; 4]) -> [u64; 4] {
    use std::arch::x86_64::*;
    unsafe {
        let data_vec = _mm256_loadu_si256(data.as_ptr() as *const __m256i);
        let mut mask_vec = _mm256_loadu_si256(mask.as_ptr() as *const __m256i);
        let mut result_vec = _mm256_setzero_si256();
        let mut block_mask_vec = _mm256_setzero_si256();
        let mut pos_bit_vec = _mm256_set1_epi8(1);
        let mut popcnt_vec = _mm256_setzero_si256();
        // in-byte processing
        while _mm256_testz_si256(mask_vec, mask_vec) == 0 {
            let lsb_vec =
                _mm256_and_si256(mask_vec, _mm256_sub_epi8(_mm256_setzero_si256(), mask_vec));
            let data_and_lsb = _mm256_and_si256(data_vec, lsb_vec);
            let mask_zero_mask = _mm256_cmpeq_epi8(lsb_vec, _mm256_setzero_si256());
            let cmp = _mm256_cmpeq_epi8(data_and_lsb, _mm256_setzero_si256());
            result_vec = _mm256_or_si256(result_vec, _mm256_andnot_si256(cmp, pos_bit_vec));
            pos_bit_vec = _mm256_slli_epi64(pos_bit_vec, 1);
            block_mask_vec = _mm256_or_si256(
                block_mask_vec,
                _mm256_blendv_epi8(pos_bit_vec, _mm256_setzero_si256(), mask_zero_mask),
            );
            mask_vec = _mm256_xor_si256(mask_vec, lsb_vec);
            popcnt_vec = _mm256_add_epi8(
                popcnt_vec,
                _mm256_add_epi8(mask_zero_mask, _mm256_set1_epi8(1)),
            );
        }
        // whole-word processing
        let mut data_vec = result_vec;
        let mut mask_vec = block_mask_vec;
        let mut shift_vec = {
            let x = _mm256_add_epi64(popcnt_vec, _mm256_slli_epi64(popcnt_vec, 8));
            let x = _mm256_add_epi64(x, _mm256_slli_epi64(x, 16));
            let x = _mm256_add_epi64(x, _mm256_slli_epi64(x, 32));
            _mm256_slli_epi64(x, 8)
        };
        let mut result_vec = _mm256_setzero_si256();
        let low_byte_mask = _mm256_set1_epi64x(0xFF);
        for _ in 0..8 {
            let group_vec = _mm256_and_si256(data_vec, low_byte_mask);
            let shifted_vec =
                _mm256_sllv_epi64(group_vec, _mm256_and_si256(shift_vec, low_byte_mask));
            result_vec = _mm256_or_si256(result_vec, shifted_vec);
            data_vec = _mm256_srli_epi64(data_vec, 8);
            shift_vec = _mm256_srli_epi64(shift_vec, 8);
        }
        let mut result = [0u64; 4];
        _mm256_storeu_si256(result.as_mut_ptr() as *mut __m256i, result_vec);
        result
    }
}

#[inline(always)]
pub fn pext_board_lower_u64x4(data: [Bitboard; 4], mask: [Bitboard; 4]) -> [u64; 4] {
    let bits0 = pext_u64x4(
        [data[0].0, data[1].0, data[2].0, data[3].0],
        [mask[0].0, mask[1].0, mask[2].0, mask[3].0],
    );
    // Since PEXT for the upper bits has only 18 bits set at most, it is expected to finish quickly.
    let bits1 = pext_u64x4(
        [data[0].1, data[1].1, data[2].1, data[3].1],
        [mask[0].1, mask[1].1, mask[2].1, mask[3].1],
    );
    let shift0 = mask[0].0.count_ones();
    let shift1 = mask[1].0.count_ones();
    let shift2 = mask[2].0.count_ones();
    let shift3 = mask[3].0.count_ones();
    [
        bits0[0] | bits1[0].wrapping_shl(shift0),
        bits0[1] | bits1[1].wrapping_shl(shift1),
        bits0[2] | bits1[2].wrapping_shl(shift2),
        bits0[3] | bits1[3].wrapping_shl(shift3),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Bitboard;

    #[test]
    fn test_pext_u64() {
        let data = 0x1234_5678_9ABC_DEF0u64;
        let mask = 0x0F0F_0F0F_0F0F_0F0Fu64;
        let result = pext_u64(data, mask);
        assert_eq!(result, 0x0000_0000_2468_ACE0u64);
    }

    #[test]
    fn test_pext_u64x4() {
        let data = [
            0x1234_5678_9ABC_DEF0u64,
            0x468A_CF01_3579_BDE2u64,
            0x9CF2_58BE_147A_D036u64,
            0x048C_159D_26AE_37BFu64,
        ];
        let mask = [
            0x0F0F_0F0F_0F0F_0F0Fu64,
            0xCCCC_AAAA_5555_3333u64,
            0xFFFF_0000_FFFF_0000u64,
            0xBEEF_CAFE_BEEF_CAFEu64,
        ];
        let expected = [
            0x0000_0000_2468_ACE0u64,
            0x0000_0000_5AB0_7DDAu64,
            0x0000_0000_9CF2_147Au64,
            0x0000_0A60_4E4E_F0DFu64,
        ];
        let result = pext_u64x4(data, mask);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_pext_board_lower_u64x4() {
        let data = [
            Bitboard(0x1234_5678_9ABC_DEF0u64, 0x1_2345u64),
            Bitboard(0x468A_CF01_3579_BDE2u64, 0x0_68ACu64),
            Bitboard(0x1CF2_58BE_147A_D036u64, 0x1_CF25u64),
            Bitboard(0x048C_159D_26AE_37BFu64, 0x0_48C1u64),
        ];
        let mask = [
            Bitboard(0x0F0F_0F0F_0F0F_0F0Fu64, 0x0_F0F0u64),
            Bitboard(0x4CCC_AAAA_5555_3333u64, 0x3_CCCCu64),
            Bitboard(0x7FFF_0000_FFFF_0000u64, 0x3_FFFFu64),
            Bitboard(0x3EEF_CAFE_BEEF_CAFEu64, 0x3_EEFCu64),
        ];
        let expected = [
            0x0000_0024_2468_ACE0u64,
            0x0000_0035_DAB0_7DDAu64,
            0x0000_E792_9CF2_147Au64,
            0x0298_0A60_4E4E_F0DFu64,
        ];
        let result = pext_board_lower_u64x4(data, mask);
        assert_eq!(result, expected);
    }
}
