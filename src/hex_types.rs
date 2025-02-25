use serde::{Deserialize, Deserializer};
use std::borrow::Cow;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::str::FromStr;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct HexU8(pub u8);

impl Display for HexU8 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "${:02X}", self.0)
    }
}

impl Debug for HexU8 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self::Display::fmt(&self, f)
    }
}

impl fmt::UpperHex for HexU8 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:02X}", self.0)
    }
}

impl fmt::LowerHex for HexU8 {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:02x}", self.0)
    }
}

impl From<u8> for HexU8 {
    fn from(val: u8) -> HexU8 {
        HexU8(val)
    }
}

impl From<HexU8> for u8 {
    fn from(val: HexU8) -> u8 {
        val.0
    }
}

impl FromStr for HexU8 {
    type Err = std::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        u8::from_str_radix(s, 16).map(HexU8)
    }
}

impl<'de> Deserialize<'de> for HexU8 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s: Cow<str> = Deserialize::deserialize(deserializer)?;
        FromStr::from_str(&s).map_err(serde::de::Error::custom)
    }
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct HexU16(pub u16);

impl Display for HexU16 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "${:04X}", self.0)
    }
}

impl Debug for HexU16 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&self, f)
    }
}

impl fmt::UpperHex for HexU16 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:04X}", self.0)
    }
}

impl fmt::LowerHex for HexU16 {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:04x}", self.0)
    }
}

impl From<u16> for HexU16 {
    fn from(val: u16) -> HexU16 {
        HexU16(val)
    }
}

impl From<HexU16> for u16 {
    fn from(val: HexU16) -> u16 {
        val.0
    }
}

impl FromStr for HexU16 {
    type Err = std::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        u16::from_str_radix(s, 16).map(HexU16)
    }
}

impl<'de> Deserialize<'de> for HexU16 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s: Cow<str> = Deserialize::deserialize(deserializer)?;
        FromStr::from_str(&s).map_err(serde::de::Error::custom)
    }
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct HexU24(pub u32);

impl Display for HexU24 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "${:06X}", self.0)
    }
}

impl Debug for HexU24 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&self, f)
    }
}

impl fmt::UpperHex for HexU24 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:06X}", self.0)
    }
}

impl fmt::LowerHex for HexU24 {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:06x}", self.0)
    }
}

impl From<u32> for HexU24 {
    fn from(val: u32) -> HexU24 {
        HexU24(val & 0xFFFFFF)
    }
}

impl From<HexU24> for u32 {
    fn from(val: HexU24) -> u32 {
        val.0
    }
}

impl FromStr for HexU24 {
    type Err = std::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        u32::from_str_radix(s, 16).map(HexU24)
    }
}

impl<'de> Deserialize<'de> for HexU24 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s: Cow<str> = Deserialize::deserialize(deserializer)?;
        FromStr::from_str(&s).map_err(serde::de::Error::custom)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum HexValue {
    Byte(HexU8),
    Word(HexU16),
    Long(HexU24),
}

impl HexValue {
    pub fn data_directive(&self) -> &'static str {
        match self {
            HexValue::Byte(_) => "db",
            HexValue::Word(_) => "dw",
            HexValue::Long(_) => "dl",
        }
    }
}

impl Display for HexValue {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            HexValue::Byte(x) => Display::fmt(&x, f),
            HexValue::Word(x) => Display::fmt(&x, f),
            HexValue::Long(x) => Display::fmt(&x, f),
        }
    }
}

impl Debug for HexValue {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&self, f)
    }
}

impl fmt::UpperHex for HexValue {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            HexValue::Byte(x) => fmt::UpperHex::fmt(&x, f),
            HexValue::Word(x) => fmt::UpperHex::fmt(&x, f),
            HexValue::Long(x) => fmt::UpperHex::fmt(&x, f),
        }
    }
}

impl fmt::LowerHex for HexValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            HexValue::Byte(x) => fmt::LowerHex::fmt(&x, f),
            HexValue::Word(x) => fmt::LowerHex::fmt(&x, f),
            HexValue::Long(x) => fmt::LowerHex::fmt(&x, f),
        }
    }
}

impl From<HexU8> for HexValue {
    fn from(val: HexU8) -> HexValue {
        HexValue::Byte(val)
    }
}

impl From<HexU16> for HexValue {
    fn from(val: HexU16) -> HexValue {
        HexValue::Word(val)
    }
}

impl From<HexU24> for HexValue {
    fn from(val: HexU24) -> HexValue {
        HexValue::Long(val)
    }
}

impl FromStr for HexValue {
    type Err = std::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let val = u32::from_str_radix(s, 16)?;
        Ok(match s.chars().count() {
            ..=2 => HexValue::Byte(HexU8(val as u8)),
            ..=4 => HexValue::Word(HexU16(val as u16)),
            _ => HexValue::Long(HexU24(val as u32)),
        })
    }
}

impl<'de> Deserialize<'de> for HexValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s: Cow<str> = Deserialize::deserialize(deserializer)?;
        FromStr::from_str(&s).map_err(serde::de::Error::custom)
    }
}
