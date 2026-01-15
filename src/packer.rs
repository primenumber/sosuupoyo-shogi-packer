use crate::Position;

pub const BUFFER_SIZE: usize = 32;

pub trait Packer {
    fn pack(&self, position: &Position, buffer: &mut [u8; BUFFER_SIZE]) -> Option<()>;
    fn unpack(&self, buffer: &[u8; BUFFER_SIZE], position: &mut Position) -> Option<()>;
}
