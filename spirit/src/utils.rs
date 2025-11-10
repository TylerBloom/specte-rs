use std::ops::Add;
use std::ops::AddAssign;
use std::ops::BitAndAssign;
use std::ops::BitOrAssign;
use std::ops::BitXorAssign;
use std::ops::Rem;
use std::ops::Sub;
use std::ops::SubAssign;

use derive_more::derive::Not;
use serde::Deserialize;
use serde::Deserializer;
use serde::Serialize;
use serde::Serializer;
use serde::de::Error;
use serde::ser::SerializeSeq;

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
        .map_err(|e| De::Error::custom(format!("{e:?}")))?
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
    PartialOrd,
    Ord,
    Serialize,
    Deserialize,
    Not,
    derive_more::Display,
    derive_more::UpperHex,
    derive_more::BitAndAssign,
    derive_more::BitOrAssign,
    derive_more::BitXorAssign,
)]
#[display("{_0}")]
pub struct Wrapping<T>(pub T);

impl From<u8> for Wrapping<u8> {
    fn from(value: u8) -> Self {
        Self(value)
    }
}

impl From<u16> for Wrapping<u16> {
    fn from(value: u16) -> Self {
        Self(value)
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

impl<T: PartialEq> PartialEq<T> for Wrapping<T> {
    fn eq(&self, other: &T) -> bool {
        &self.0 == other
    }
}

impl<T: PartialOrd> PartialOrd<T> for Wrapping<T> {
    fn partial_cmp(&self, other: &T) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(other)
    }
}

impl<T> Rem<T> for Wrapping<T>
where
    T: Rem<Output = T>,
{
    type Output = T;

    fn rem(self, rhs: T) -> Self::Output {
        self.0 % rhs
    }
}
