use std::ops::{Add, AddAssign, BitAndAssign, BitOrAssign, BitXorAssign, Sub, SubAssign};

use derive_more::derive::Not;
use serde::{de::Error, ser::SerializeSeq, Deserialize, Deserializer, Serialize, Serializer};

use heapless::Vec as InlineVec;

pub(crate) fn serialize_slices_as_one<Sl: AsRef<[u8]>, Se: Serializer>(
    slices: &[Sl],
    ser: Se,
) -> Result<Se::Ok, Se::Error> {
    let mut seq = ser.serialize_seq(Some(slices.len()))?;
    slices
        .iter()
        .map(AsRef::as_ref)
        .try_for_each(|b| seq.serialize_element(b))
        .and_then(|()| seq.end())
}

pub(crate) fn deserialize_slices_as_one<
    'de,
    const N: usize,
    const M: usize,
    De: Deserializer<'de>,
>(
    de: De,
) -> Result<[[u8; N]; M], De::Error> {
    Ok(InlineVec::<InlineVec<u8, N>, M>::deserialize(de)?
        .into_array()
        .map_err(|e| De::Error::custom(format!("")))?
        .map(|data| data.into_array().unwrap()))
}

#[derive(
    Debug,
    Default,
    Hash,
    Clone,
    Copy,
    PartialEq,
    Eq,
    derive_more::Display,
    Serialize,
    Deserialize,
    derive_more::UpperHex,
    Not,
    derive_more::BitAndAssign,
    derive_more::BitOrAssign,
    derive_more::BitXorAssign,
)]
#[display("{_0}")]
pub struct Wrapping<T>(pub T);

impl Wrapping<u16> {
    pub(crate) fn to_be_bytes(self) -> [Wrapping<u8>; 2] {
        self.0.to_be_bytes().map(Wrapping)
    }

    pub(crate) fn to_ne_bytes(self) -> [Wrapping<u8>; 2] {
        self.0.to_ne_bytes().map(Wrapping)
    }

    pub(crate) fn to_le_bytes(self) -> [Wrapping<u8>; 2] {
        self.0.to_le_bytes().map(Wrapping)
    }
}

impl BitOrAssign<u8> for Wrapping<u8> {
    fn bitor_assign(&mut self, rhs: u8) {
        self.0 |= rhs;
    }
}

impl BitAndAssign<u8> for Wrapping<u8> {
    fn bitand_assign(&mut self, rhs: u8) {
        self.0 &= rhs;
    }
}

impl BitXorAssign<u8> for Wrapping<u8> {
    fn bitxor_assign(&mut self, rhs: u8) {
        self.0 ^= rhs;
    }
}

impl Add<u8> for Wrapping<u8> {
    type Output = Self;

    fn add(self, rhs: u8) -> Self::Output {
        Self(self.0.wrapping_add(rhs))
    }
}

impl Add<Self> for Wrapping<u8> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        self + rhs.0
    }
}

impl<T> AddAssign<T> for Wrapping<u8>
where
    Self: Add<T, Output = Self>,
{
    fn add_assign(&mut self, rhs: T) {
        *self = *self + rhs
    }
}

impl Sub<u8> for Wrapping<u8> {
    type Output = Self;

    fn sub(self, rhs: u8) -> Self::Output {
        Self(self.0.wrapping_sub(rhs))
    }
}

impl Sub<Self> for Wrapping<u8> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self - rhs.0
    }
}

impl<T> SubAssign<T> for Wrapping<u8>
where
    Self: Sub<T, Output = Self>,
{
    fn sub_assign(&mut self, rhs: T) {
        *self = *self - rhs
    }
}

impl Add<u16> for Wrapping<u16> {
    type Output = Self;

    fn add(self, rhs: u16) -> Self::Output {
        Self(self.0.wrapping_add(rhs))
    }
}

impl Add<Self> for Wrapping<u16> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        self + rhs.0
    }
}

impl Add<u8> for Wrapping<u16> {
    type Output = Self;

    fn add(self, rhs: u8) -> Self::Output {
        self + rhs as u16
    }
}
impl<T> AddAssign<T> for Wrapping<u16>
where
    Self: Add<T, Output = Self>,
{
    fn add_assign(&mut self, rhs: T) {
        *self = *self + rhs
    }
}

impl Sub<u16> for Wrapping<u16> {
    type Output = Self;

    fn sub(self, rhs: u16) -> Self::Output {
        Self(self.0.wrapping_sub(rhs))
    }
}

impl Sub<u8> for Wrapping<u16> {
    type Output = Self;

    fn sub(self, rhs: u8) -> Self::Output {
        self - rhs as u16
    }
}

impl Sub<Self> for Wrapping<u16> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self - rhs.0
    }
}

impl<T> SubAssign<T> for Wrapping<u16>
where
    Self: Sub<T, Output = Self>,
{
    fn sub_assign(&mut self, rhs: T) {
        *self = *self - rhs
    }
}
