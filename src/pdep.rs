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

pub fn pdep_board_lower_u64(data: u64, mask: Bitboard) -> Bitboard {
    let shift = mask.0.count_ones();
    let bits0 = pdep_u64(data, mask.0);
    let bits1 = pdep_u64(data.wrapping_shr(shift), mask.1);
    Bitboard(bits0, bits1)
}

pub fn pdep_board(data: Bitboard, mask: Bitboard) -> Bitboard {
    let shift = mask.0.count_ones();
    let bits0 = pdep_u64(data.0, mask.0);
    let bits1 = pdep_u64(
        data.0.wrapping_shr(shift) | data.1.wrapping_shl(63 - shift),
        mask.1,
    );
    Bitboard(bits0, bits1)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pext::pext_u64;

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
}
