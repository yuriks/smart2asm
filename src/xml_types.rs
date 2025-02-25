use crate::hex_types::{HexU16, HexU24, HexU8, HexValue};
use quick_xml::impl_deserialize_for_internally_tagged_enum;
use serde::de::{DeserializeOwned, IntoDeserializer};
use serde::{Deserialize, Deserializer};
use std::borrow::Cow;
use std::str::FromStr;

macro_rules! make_list_unwrapper {
    ($fn_name:ident, $type:ty, $el_name:literal) => {
        fn $fn_name<'de, D: Deserializer<'de>>(deserializer: D) -> Result<$type, D::Error> {
            #[derive(Deserialize)]
            struct Holder {
                #[serde(rename = $el_name, default)]
                children: $type,
            }
            Ok(Holder::deserialize(deserializer)?.children)
        }
    };
}

fn split_xml_whitespace<'de, T: DeserializeOwned, D: Deserializer<'de>>(
    deserializer: D,
) -> Result<Vec<T>, D::Error> {
    let s: Cow<str> = Deserialize::deserialize(deserializer)?;
    s.split_ascii_whitespace()
        .map(|s| T::deserialize(s.into_deserializer()))
        .collect()
}

#[derive(Deserialize, Debug)]
pub struct SaveInDoor {
    #[serde(rename = "@roomarea")]
    pub room_area: HexU8,
    #[serde(rename = "@roomindex")]
    pub room_index: HexU8,
    #[serde(rename = "@doorindex")]
    pub door_index: HexU8,
}

#[derive(Deserialize, Debug)]
pub struct SaveRoom {
    pub saveindex: HexU8,
    pub indoor: SaveInDoor,          // TODO: Flatten into this struct?
    pub unused: [Option<HexU16>; 2], // SMART writes this twice to the XML, but it seems like a bug
    pub screenx: HexU16,
    pub screeny: HexU16,
    pub samusx: HexU16,
    pub samusy: HexU16,
}

#[derive(Deserialize, Debug)]
pub struct ToRoom {
    #[serde(rename = "@area")]
    pub area: HexU8,
    #[serde(rename = "@index")]
    pub index: HexU8,
}

#[derive(Deserialize, Debug)]
pub struct CodeOp {
    #[serde(rename = "@OP")]
    pub op: HexU8,
    #[serde(rename = "@ARG")]
    pub arg: Option<HexValue>,
}

#[derive(Deserialize, Debug)]
pub struct DoorCode {
    // TODO: These three are mutually exclusive, but can't use an enum because Code repeats
    #[serde(rename = "Code", default)]
    pub ops: Vec<CodeOp>,
    #[serde(rename = "ScrollData")]
    pub scroll_data: Option<ScrollDataChange>,
    #[serde(rename = "$text")]
    pub address: Option<HexU16>,
}

#[derive(Deserialize, Debug)]
pub struct Door {
    pub toroom: ToRoom, // TODO: Flatten into this struct?
    pub bitflag: HexU8,
    pub direction: HexU8,
    pub tilex: HexU8,
    pub tiley: HexU8,
    pub screenx: HexU8,
    pub screeny: HexU8,
    pub distance: HexU16,
    pub doorcode: DoorCode,
}

#[derive(Deserialize, Debug)]
pub struct Fx1 {
    #[serde(rename = "@default", default)]
    pub default: bool,
    #[serde(rename = "@roomarea")]
    pub roomarea: Option<HexU8>,
    #[serde(rename = "@roomindex")]
    pub roomindex: Option<HexU8>,
    #[serde(rename = "@fromdoor")]
    pub fromdoor: Option<HexU8>,

    pub surfacestart: HexU16,
    pub surfacenew: HexU16,
    pub surfacespeed: HexU16,
    pub surfacedelay: HexU8,
    #[serde(rename = "type")]
    pub type_: HexU8,
    #[serde(rename = "transparency1_A")]
    pub transparency1_a: HexU8,
    #[serde(rename = "transparency2_B")]
    pub transparency2_b: HexU8,
    #[serde(rename = "liquidflags_C")]
    pub liquidflags_c: HexU8,
    pub paletteflags: HexU8,
    pub animationflags: HexU8,
    pub paletteblend: HexU8,
}

#[derive(Deserialize, Debug)]
pub struct Enemy {
    #[serde(rename = "ID")]
    pub id: HexU16,
    #[serde(rename = "X")]
    pub x: HexU16,
    #[serde(rename = "Y")]
    pub y: HexU16,
    pub tilemap: HexU16,
    pub special: HexU16,
    pub gfx: HexU16,
    pub speed: HexU16,
    pub speed2: HexU16,
}

#[derive(Deserialize, Debug)]
pub struct EnemiesList {
    #[serde(rename = "@killcount")]
    pub kill_count: HexU8,
    #[serde(rename = "Enemy", default)]
    pub enemy: Vec<Enemy>,
}

#[derive(Deserialize, Debug)]
pub struct EnemyType {
    #[serde(rename = "GFX")]
    pub gfx: HexU16,
    pub palette: HexU16,
}

#[derive(Deserialize, Debug, Eq, PartialEq)]
pub enum LayerType {
    Layer2,
    #[serde(rename = "BGData")]
    BgData,
}

#[derive(Deserialize, Debug)]
pub struct ScrollData {
    // TODO: These should be mutually exclusive
    #[serde(rename = "@const")]
    pub const_: Option<HexU16>,
    #[serde(default, rename = "$text", deserialize_with = "split_xml_whitespace")]
    pub data: Vec<HexU8>,
}

#[derive(Deserialize, Debug)]
pub enum ScrollDataChangeEntry {
    Change {
        #[serde(rename = "@screen")]
        screen: HexU8,
        #[serde(rename = "@scroll")]
        scroll: HexU8,
    },
}

#[derive(Deserialize, Debug)]
pub struct ScrollDataChange {
    #[serde(rename = "$value")]
    pub entries: Vec<ScrollDataChangeEntry>,
}

#[derive(Deserialize, Debug)]
pub struct Plm {
    #[serde(rename = "type")]
    pub type_: HexU16,
    pub x: HexU8,
    pub y: HexU8,
    // TODO: Mutually exclusive(?) with scroll_data
    pub arg: Option<HexU16>,
    #[serde(rename = "ScrollData")]
    pub scroll_data: Option<ScrollDataChange>,
}

#[derive(Debug)]
pub enum DataOrAddress {
    Data(Vec<HexU16>),
    Address(HexU24),
}

impl<'de> Deserialize<'de> for DataOrAddress {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s: Cow<str> = Deserialize::deserialize(deserializer)?;
        if let Ok(addr) = HexU24::from_str(&s) {
            return Ok(DataOrAddress::Address(addr));
        }
        let vals = s
            .split_ascii_whitespace()
            .map(|t| HexU16::from_str(t))
            .collect::<Result<Vec<HexU16>, _>>()
            .map_err(serde::de::Error::custom)?;
        Ok(DataOrAddress::Data(vals))
    }
}

#[derive(Debug, Deserialize)]
pub enum DecompSection {
    GFX,
    GFX3,
    Tiles2,
    Tiles1,
    Tiles3,
}

#[derive(Debug)]
pub enum BgDataEntry {
    // #[serde(rename = "COPY")]
    Copy {
        // #[serde(rename = "SOURCE")]
        source: DataOrAddress,
        // #[serde(rename = "DEST")]
        dest: HexU16,
        // #[serde(rename = "SIZE")]
        size: HexU16,
    },
    // #[serde(rename = "DECOMP")]
    Decomp {
        // #[serde(rename = "SOURCE", deserialize_with = "split_xml_whitespace")]
        source: DataOrAddress,
        // #[serde(rename = "DEST")]
        dest: HexU16,
        // #[serde(rename = "Section")]
        section: Option<DecompSection>,
    },
    // #[serde(rename = "L3COPY")]
    L3Copy {
        // #[serde(rename = "SOURCE")]
        source: DataOrAddress,
        // #[serde(rename = "DEST")]
        dest: HexU16,
        // #[serde(rename = "SIZE")]
        size: HexU16,
    },
    // #[serde(rename = "CLEAR2")]
    Clear2,
    // #[serde(rename = "CLEARALL")]
    ClearAll,
    // #[serde(rename = "DDBCOPY")]
    DdbCopy {
        // #[serde(rename = "DDB")]
        ddb: HexU16,
        // #[serde(rename = "SOURCE")]
        source: DataOrAddress,
        // #[serde(rename = "DEST")]
        dest: HexU16,
        // #[serde(rename = "SIZE")]
        size: HexU16,
    },
}

// This unfortunately needs to be duplicated.
// To avoid it, maybe I can make a macro to call this macro...
impl_deserialize_for_internally_tagged_enum! {
    BgDataEntry, "@Type",
    ("COPY" => Copy {
        #[serde(rename = "SOURCE")]
        source: DataOrAddress,
        #[serde(rename = "DEST")]
        dest: HexU16,
        #[serde(rename = "SIZE")]
        size: HexU16,
    }),
    ("DECOMP" => Decomp {
        #[serde(rename = "SOURCE")]
        source: DataOrAddress,
        #[serde(rename = "DEST")]
        dest: HexU16,
        #[serde(rename = "Section")]
        section: Option<DecompSection>,
    }),
    ("L3COPY" => L3Copy {
        #[serde(rename = "SOURCE")]
        source: DataOrAddress,
        #[serde(rename = "DEST")]
        dest: HexU16,
        #[serde(rename = "SIZE")]
        size: HexU16,
    }),
    ("CLEAR2" => Clear2),
    ("CLEARALL" => ClearAll),
    ("DDBCOPY" => DdbCopy {
        #[serde(rename = "DDB")]
        ddb: HexU16,
        #[serde(rename = "SOURCE")]
        source: DataOrAddress,
        #[serde(rename = "DEST")]
        dest: HexU16,
        #[serde(rename = "SIZE")]
        size: HexU16,
    }),
}

#[derive(Deserialize, Debug)]
pub struct Screen<T> {
    #[serde(rename = "@X")]
    pub x: HexU8,
    #[serde(rename = "@Y")]
    pub y: HexU8,
    #[serde(
        rename = "$text",
        bound(deserialize = "T: DeserializeOwned"),
        deserialize_with = "split_xml_whitespace"
    )]
    pub data: Vec<T>,
}

#[derive(Deserialize, Debug)]
pub struct LevelDataLayer<T> {
    #[serde(rename = "Screen", bound(deserialize = "T: DeserializeOwned"))]
    pub screens: Vec<Screen<T>>,
}

#[derive(Deserialize, Debug)]
pub struct LevelData {
    #[serde(rename = "@Width")]
    pub width: HexU8,
    #[serde(rename = "@Height")]
    pub height: HexU8,

    #[serde(rename = "Layer1")]
    pub layer1: LevelDataLayer<HexU16>,
    #[serde(rename = "BTS")]
    pub bts: LevelDataLayer<HexU8>,
    #[serde(rename = "Layer2")]
    pub layer2: Option<LevelDataLayer<HexU16>>,
}

#[derive(Debug)]
pub enum StateCondition {
    Default,
    Short(HexU16),
}

impl<'de> Deserialize<'de> for StateCondition {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s: Cow<str> = Deserialize::deserialize(deserializer)?;
        if s == "default" {
            Ok(StateCondition::Default)
        } else {
            FromStr::from_str(&s)
                .map(StateCondition::Short)
                .map_err(serde::de::Error::custom)
        }
    }
}

#[derive(Deserialize, Debug)]
pub struct StateConditionArg {
    // Arg type information isn't available during parsing so the size of this parameter is unknown.
    // This might be "byte" (u8), "short" (u16), or "long" (u24), although vanilla only uses byte
    // arguments.
    #[serde(rename = "$text")]
    pub value: HexU24,
    // TODO: Subversion uses an arg type called "Door" which uses XML attributes
}

make_list_unwrapper!(unwrap_fx1_list, Vec<Fx1>, "FX1");
make_list_unwrapper!(unwrap_enemy_type_list, Vec<EnemyType>, "Enemy");
make_list_unwrapper!(unwrap_plm_list, Vec<Plm>, "PLM");
make_list_unwrapper!(unwrap_bg_data_list, Vec<BgDataEntry>, "Data");

#[derive(Deserialize, Debug)]
pub struct RoomState {
    #[serde(rename = "@condition")]
    pub condition: StateCondition,
    #[serde(rename = "Arg", default)]
    pub condition_args: Vec<StateConditionArg>,
    #[serde(rename = "LevelData")]
    pub level_data: LevelData,

    #[serde(rename = "GFXset")]
    pub gfx_set: HexU8,
    pub music: HexU16,
    #[serde(rename = "FX1s", deserialize_with = "unwrap_fx1_list")]
    pub fx1s: Vec<Fx1>,
    #[serde(rename = "Enemies")]
    pub enemies: EnemiesList,
    #[serde(rename = "EnemyTypes", deserialize_with = "unwrap_enemy_type_list")]
    pub enemy_types: Vec<EnemyType>,

    pub layer2_type: LayerType,
    pub layer2_xscroll: HexU8,
    pub layer2_yscroll: HexU8,
    #[serde(rename = "ScrollData")]
    pub scroll_data: ScrollData,
    pub roomvar: HexU16,
    #[serde(rename = "FX2")]
    pub fx2: HexU16,

    #[serde(rename = "PLMs", deserialize_with = "unwrap_plm_list")]
    pub plms: Vec<Plm>,
    #[serde(rename = "BGData", deserialize_with = "unwrap_bg_data_list")]
    pub bg_data: Vec<BgDataEntry>,
    pub layer1_2: HexU16,
}

#[derive(Deserialize, Debug)]
pub enum DoorEntry {
    Elevator,
    Door(Door),
}

make_list_unwrapper!(unwrap_saves_list, Vec<SaveRoom>, "SaveRoom");
make_list_unwrapper!(unwrap_door_entry_list, Vec<DoorEntry>, "$value");
make_list_unwrapper!(unwrap_room_state_list, Vec<RoomState>, "State");

#[derive(Deserialize, Debug)]
pub struct Room {
    pub index: HexU8,
    pub area: HexU8,
    pub x: HexU8,
    pub y: HexU8,
    pub width: HexU8,
    pub height: HexU8,
    pub upscroll: HexU8,
    pub dnscroll: HexU8,
    #[serde(rename = "specialGFX")]
    pub special_gfx: HexU8, // bitflags

    #[serde(rename = "Saves", deserialize_with = "unwrap_saves_list")]
    pub saves: Vec<SaveRoom>,
    #[serde(rename = "Doors", deserialize_with = "unwrap_door_entry_list")]
    pub doors: Vec<DoorEntry>,
    #[serde(rename = "States", deserialize_with = "unwrap_room_state_list")]
    pub states: Vec<RoomState>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use quick_xml::de::{from_reader, from_str};

    const CRATERIA_PB_ROOM_XML: &str =
        include_str!("../../smart_xml/Export/Rooms/CRATERIA POWER BOMB ROOM.xml");

    #[test]
    fn test_crateria_pbs_room() {
        let room_: Room = from_str(CRATERIA_PB_ROOM_XML).unwrap();
        //eprintln!("{:#?}", room);
    }

    const LANDING_SITE_XML: &str = include_str!("../../smart_xml/Export/Rooms/LANDING SITE.xml");

    #[test]
    fn test_landing_site_room() {
        let room_: Room = from_str(LANDING_SITE_XML).unwrap();
        //eprintln!("{:#?}", room);
    }

    #[test]
    fn test_landing_site_room_buf() {
        let room_: Room = from_reader(LANDING_SITE_XML.as_bytes()).unwrap();
        //eprintln!("{:#?}", room);
    }
}
