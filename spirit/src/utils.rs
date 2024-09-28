use serde::{de::Error, ser::SerializeSeq, Deserialize, Deserializer, Serializer};

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
