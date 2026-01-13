use crate::Bitboard;

#[cfg(target_feature = "bmi2")]
pub fn pdep_u64(data: u64, mask: u64) -> u64 {
    use std::arch::x86_64::*;
    unsafe { _pdep_u64(data, mask) }
}

#[cfg(not(target_feature = "bmi2"))]
pub fn pdep_u64(mut data: u64, mut mask: u64) -> u64 {
    let mut result = 0;
    while mask != 0 {
        let lsb_bit = mask & mask.wrapping_neg();
        if data & 1 != 0 {
            result |= lsb_bit;
        }
        data = data.wrapping_shr(1);
        mask ^= lsb_bit;
    }
    result
}

#[cfg(any(target_feature = "bmi2", not(target_feature = "avx2")))]
pub fn pdep_u64x4(data: [u64; 4], mask: [u64; 4]) -> [u64; 4] {
    [
        pdep_u64(data[0], mask[0]),
        pdep_u64(data[1], mask[1]),
        pdep_u64(data[2], mask[2]),
        pdep_u64(data[3], mask[3]),
    ]
}

#[cfg(all(not(target_feature = "bmi2"), target_feature = "avx2"))]
pub fn pdep_u64x4(data: [u64; 4], mask: [u64; 4]) -> [u64; 4] {
    use std::arch::x86_64::*;
    unsafe {
        let mut data_vec = _mm256_loadu_si256(data.as_ptr() as *const __m256i);
        let mut mask_vec = _mm256_loadu_si256(mask.as_ptr() as *const __m256i);
        let mut result_vec = _mm256_setzero_si256();
        let one_vec = _mm256_set1_epi64x(1);
        while _mm256_testz_si256(mask_vec, mask_vec) == 0 {
            let lsb_vec =
                _mm256_and_si256(mask_vec, _mm256_sub_epi64(_mm256_setzero_si256(), mask_vec));
            // Check if data & 1 != 0
            let data_lsb = _mm256_and_si256(data_vec, one_vec);
            let cmp = _mm256_cmpeq_epi64(data_lsb, _mm256_setzero_si256());
            result_vec = _mm256_or_si256(result_vec, _mm256_andnot_si256(cmp, lsb_vec));
            data_vec = _mm256_srli_epi64(data_vec, 1);
            mask_vec = _mm256_xor_si256(mask_vec, lsb_vec);
        }
        let mut result = [0u64; 4];
        _mm256_storeu_si256(result.as_mut_ptr() as *mut __m256i, result_vec);
        result
    }
}

/// Inverse of `pext_board_lower_u64x4`.
/// Deposits lower bits of data into bitboard positions specified by mask.
pub fn pdep_board_lower_u64x4(data: [u64; 4], mask: [Bitboard; 4]) -> [Bitboard; 4] {
    let shift0 = mask[0].0.count_ones();
    let shift1 = mask[1].0.count_ones();
    let shift2 = mask[2].0.count_ones();
    let shift3 = mask[3].0.count_ones();
    let bits0 = pdep_u64x4(
        [data[0], data[1], data[2], data[3]],
        [mask[0].0, mask[1].0, mask[2].0, mask[3].0],
    );
    let bits1 = pdep_u64x4(
        [
            data[0].wrapping_shr(shift0),
            data[1].wrapping_shr(shift1),
            data[2].wrapping_shr(shift2),
            data[3].wrapping_shr(shift3),
        ],
        [mask[0].1, mask[1].1, mask[2].1, mask[3].1],
    );
    [
        Bitboard(bits0[0], bits1[0]),
        Bitboard(bits0[1], bits1[1]),
        Bitboard(bits0[2], bits1[2]),
        Bitboard(bits0[3], bits1[3]),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pext::{pext_board_lower_u64x4, pext_u64};

    #[test]
    fn test_pdep_u64_basic() {
        // PDEP deposits bits from data into positions specified by mask
        // data = 0b1011, mask = 0b11110000 -> result = 0b10110000
        assert_eq!(pdep_u64(0b1011, 0b11110000), 0b10110000);

        // data = 0b101, mask = 0b10101010
        // mask bits set at positions: 1, 3, 5, 7
        // data bits 0,1,2 = 1,0,1 placed at positions 1, 3, 5 -> 0b00100010
        assert_eq!(pdep_u64(0b101, 0b10101010), 0b00100010);

        // Empty mask
        assert_eq!(pdep_u64(0xFFFF, 0), 0);

        // Full mask (lower 8 bits)
        assert_eq!(pdep_u64(0xAB, 0xFF), 0xAB);
    }

    #[test]
    fn test_pdep_pext_roundtrip() {
        // PDEP and PEXT are inverse operations
        let test_cases = [
            (0x123456789ABCDEF0u64, 0xF0F0F0F0F0F0F0F0u64),
            (0xAAAAAAAAAAAAAAAAu64, 0x5555555555555555u64),
            (0xFFFFFFFFFFFFFFFFu64, 0x00FF00FF00FF00FFu64),
            (0x0123456789ABCDEFu64, 0xFFFFFFFF00000000u64),
        ];

        for (data, mask) in test_cases {
            let extracted = pext_u64(data, mask);
            let deposited = pdep_u64(extracted, mask);
            // After roundtrip, we should get the original data masked
            assert_eq!(deposited, data & mask);
        }
    }

    #[test]
    fn test_pdep_u64x4() {
        let data = [0b1011u64, 0b101u64, 0xABu64, 0x12u64];
        let mask = [0b11110000u64, 0b10101010u64, 0xFFu64, 0xF0F0u64];
        let result = pdep_u64x4(data, mask);

        assert_eq!(result[0], pdep_u64(data[0], mask[0]));
        assert_eq!(result[1], pdep_u64(data[1], mask[1]));
        assert_eq!(result[2], pdep_u64(data[2], mask[2]));
        assert_eq!(result[3], pdep_u64(data[3], mask[3]));
    }

    #[test]
    fn test_pdep_board_lower_u64x4_roundtrip() {
        let boards = [
            Bitboard(0x123456789ABCDEF0, 0x0FEDCBA987654321),
            Bitboard(0xAAAAAAAAAAAAAAAA, 0x5555555555555555),
            Bitboard(0xFFFFFFFFFFFFFFFF, 0x0001FFFF),
            Bitboard(0x0, 0x0),
        ];
        let masks = [
            Bitboard(0xF0F0F0F0F0F0F0F0, 0x0F0F0F0F0F0F0F0F),
            Bitboard(0xFFFFFFFF00000000, 0x00000000FFFFFFFF),
            Bitboard(0xFFFFFFFFFFFFFFFF, 0x0001FFFF),
            Bitboard(0x8040201008040201, 0x0001804020100804),
        ];

        let extracted = pext_board_lower_u64x4(boards, masks);
        let deposited = pdep_board_lower_u64x4(extracted, masks);

        for i in 0..4 {
            assert_eq!(
                deposited[i],
                Bitboard(boards[i].0 & masks[i].0, boards[i].1 & masks[i].1),
                "Mismatch at index {}",
                i
            );
        }
    }
}
