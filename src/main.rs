use crate::asm::SymbolMap;
use crate::hex_types::{HexU8, HexU16, HexU24, HexValue};
use crate::xml_types::{DataOrAddress, DecompSection};
use anyhow::{Result, anyhow};
use glob::glob;
use minijinja::value::{Enumerator, Kwargs, Object, ObjectRepr};
use minijinja::{Environment, ErrorKind, State, UndefinedBehavior, Value, context, value};
use serde::ser::{SerializeSeq, SerializeStruct};
use serde::{Serialize, Serializer};
use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, HashMap};
use std::fmt::{Debug, Formatter};
use std::fs::File;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::io::BufReader;
use std::marker::PhantomData;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::{Arc, Mutex};
use std::{fs, io};

mod asm;
mod hex_types;
mod xml_types;

#[derive(Debug)]
struct DeduperEntry<T> {
    data: T,
    labels: Vec<String>,
    refcount: u32,
}

struct OwningRef<T> {
    idx: usize,
    label: String,
    marker: PhantomData<fn(T) -> T>,
}

impl<T> Clone for OwningRef<T> {
    fn clone(&self) -> Self {
        Self {
            idx: self.idx,
            label: self.label.clone(),
            marker: PhantomData,
        }
    }
}

impl<T> Debug for OwningRef<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("OwningRef")
            .field(&self.idx)
            .field(&self.label)
            .finish()
    }
}

impl<T> OwningRef<T> {
    fn label(&self) -> &str {
        &self.label
    }
}

impl<T> Hash for OwningRef<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.idx.hash(state);
    }
}

impl<T> Serialize for OwningRef<T>
where
    Self: Object + 'static,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if value::serializing_for_value() {
            return Value::from_object(self.clone()).serialize(serializer);
        }
        let mut s = serializer.serialize_struct("OwningRef", 2)?;
        s.serialize_field("idx", &self.idx)?;
        s.serialize_field("label", &self.label)?;
        s.end()
    }
}

trait RomDataHandle {
    type Target;

    fn resolve<'a>(&self, data: &'a RomData) -> Option<&'a Self::Target>;
}

impl<T> Object for OwningRef<T>
where
    Self: RomDataHandle,
    <Self as RomDataHandle>::Target: Serialize,
{
    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        match key.as_str()? {
            "label" => Some(Value::from(self.label())),
            _ => None,
        }
    }

    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Str(&["label"])
    }

    fn call(self: &Arc<Self>, state: &State, args: &[Value]) -> Result<Value, minijinja::Error> {
        let () = value::from_args(args)?;
        let internal_state = get_internal_state(state)?;
        let object = self
            .resolve(&internal_state.rom_data)
            .ok_or(ErrorKind::UndefinedError)?;
        Ok(Value::from_serialize(object))
    }

    fn render(self: &Arc<Self>, f: &mut Formatter) -> std::fmt::Result
    where
        Self: Sized + 'static,
    {
        f.write_str(self.label())
    }
}

#[derive(Debug)]
struct DataDeduper<T> {
    entries: Vec<DeduperEntry<T>>,
    hash_to_id: HashMap<u64, usize>,
}

// `derive(Default)` adds an unwanted bound on T, so this needs to be impl'd manually
impl<T> Default for DataDeduper<T> {
    fn default() -> Self {
        DataDeduper {
            entries: Vec::new(),
            hash_to_id: HashMap::new(),
        }
    }
}

impl<T: Hash> DataDeduper<T> {
    fn insert(&mut self, data: T, label: String) -> OwningRef<T> {
        let mut hasher = DefaultHasher::new();
        data.hash(&mut hasher);
        let hash = hasher.finish();

        let &mut id = self.hash_to_id.entry(hash).or_insert_with(|| {
            let id = self.entries.len();
            self.entries.push(DeduperEntry {
                data,
                labels: Vec::new(),
                refcount: 0,
            });
            id
        });

        let entry = &mut self.entries[id];
        entry.labels.push(label.clone());
        entry.refcount += 1;
        OwningRef {
            idx: id,
            label,
            marker: PhantomData,
        }
    }

    fn get(&self, id: &OwningRef<T>) -> Option<&DeduperEntry<T>> {
        self.entries.get(id.idx)
    }
}

impl<T> DataDeduper<T> {
    fn entries(&self) -> &[DeduperEntry<T>] {
        &self.entries
    }
}

impl<T: Serialize> Serialize for DataDeduper<T> {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.entries().len()))?;
        for e in self.entries() {
            seq.serialize_element(&(&e.labels, &e.data))?;
        }
        seq.end()
    }
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
struct RoomId {
    area: u8,
    room: u8,
}

impl Debug for RoomId {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("RoomId")
            .field("area", &HexU8(self.area))
            .field("room", &HexU8(self.room))
            .finish()
    }
}

impl RomDataHandle for RoomId {
    type Target = RoomHeader;

    fn resolve<'a>(&self, data: &'a RomData) -> Option<&'a Self::Target> {
        data.rooms.get(self)
    }
}

impl Serialize for RoomId
where
    Self: Object + 'static,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if value::serializing_for_value() {
            return Value::from_object(*self).serialize(serializer);
        }
        let mut s = serializer.serialize_struct("RoomId", 2)?;
        s.serialize_field("area", &self.area)?;
        s.serialize_field("room", &self.room)?;
        s.end()
    }
}

impl Object for RoomId {
    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        match key.as_str()? {
            "area" => Some(Value::from(self.area)),
            "room" => Some(Value::from(self.room)),
            _ => None,
        }
    }

    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Str(&["area", "room"])
    }

    fn call(self: &Arc<Self>, state: &State, args: &[Value]) -> Result<Value, minijinja::Error> {
        let () = value::from_args(args)?;
        let internal_state = get_internal_state(state)?;
        let object = self
            .resolve(&internal_state.rom_data)
            .ok_or(ErrorKind::UndefinedError)?;
        Ok(Value::from_serialize(object))
    }
}

#[derive(Debug, Serialize)]
struct RoomHeader {
    name: String,

    room_index: HexU8,
    area_index: HexU8,

    room_map_x: HexU8,
    room_map_y: HexU8,
    room_width: HexU8,
    room_height: HexU8,

    up_scroll: HexU8,
    down_scroll: HexU8,

    special_gfx_flags: HexU8,

    // Not going to dedup door lists yet
    exits: Vec<OwningRef<ExitType>>,
    states: Vec<RoomState>,
}

impl RoomHeader {
    fn from_xml(
        rom_data: &mut RomData,
        xml_room: &xml_types::Room,
        room_name: String,
    ) -> Result<(RoomId, RoomHeader)> {
        let room_id = RoomId {
            area: xml_room.area.0,
            room: xml_room.index.0,
        };
        let exits = xml_room
            .doors
            .iter()
            .enumerate()
            .map(|(i, x)| {
                let exit_name = format!("{room_name}_{i}");
                let exit = ExitType::from_xml(rom_data, x, &exit_name)?;
                Ok(rom_data.doors.insert(exit, format!("Door_{exit_name}")))
            })
            .collect::<Result<_>>()?;
        let states = xml_room
            .states
            .iter()
            .rev() // SMART Saves room states in reverse order
            .enumerate()
            .map(|(i, x)| RoomState::from_xml(rom_data, x, format!("{room_name}_{i}")))
            .collect::<Result<_>>()?;
        for xml_save in &xml_room.saves {
            let (save_id, save) = LoadStation::from_xml(rom_data, xml_save, room_id)?;
            let load_station =
                get_load_station_mut(&mut rom_data.load_stations_per_area, xml_room.area, save_id);
            if load_station.replace(save).is_some() {
                return Err(anyhow!(
                    "Duplicate load stations with (area={:?}, load_station={:?})",
                    xml_room.area,
                    save_id
                ));
            }
        }

        Ok((
            room_id,
            RoomHeader {
                name: room_name,
                room_index: xml_room.index,
                area_index: xml_room.area,
                room_map_x: xml_room.x,
                room_map_y: xml_room.y,
                room_width: xml_room.width,
                room_height: xml_room.height,
                up_scroll: xml_room.upscroll,
                down_scroll: xml_room.dnscroll,
                special_gfx_flags: xml_room.special_gfx,
                exits,
                states,
            },
        ))
    }
}

fn get_load_station_mut(
    stations_per_area: &mut Vec<Vec<Option<LoadStation>>>,
    area: HexU8,
    load_station: HexU8,
) -> &mut Option<LoadStation> {
    let area: usize = area.0.into();
    if area >= stations_per_area.len() {
        stations_per_area.resize_with(area + 1, Default::default);
    }
    let area_stations = &mut stations_per_area[area];

    let load_station: usize = load_station.0.into();
    if load_station >= area_stations.len() {
        area_stations.resize_with(load_station + 1, Default::default);
    }
    &mut area_stations[load_station]
}

type DoorId = (RoomId, u8 /* exit index */); // Unlike RoomHeaders, multiple ids can represent the same DoorHeader

#[derive(Debug, Hash, Serialize)]
#[serde(tag = "type")]
enum ExitType {
    Elevator,
    Door(DoorHeader),
}

#[derive(Debug, Hash, Serialize)]
struct DoorHeader {
    destination: RoomId,
    transition_flags: HexU8, // 0x80 = elevator destination, 0x40 = area change
    transition_type: HexU8,  // 0x04 = create door cap, 0x03 = direction
    doorcap_tile_x: HexU8,
    doorcap_tile_y: HexU8,
    destination_screen_x: HexU8,
    destination_screen_y: HexU8,
    samus_slide_speed: HexU16,
    door_asm: DoorAsmType,
}

impl ExitType {
    fn from_xml(
        rom_data: &mut RomData,
        xml: &xml_types::DoorEntry,
        exit_name: &str,
    ) -> Result<ExitType> {
        Ok(match xml {
            xml_types::DoorEntry::Elevator => ExitType::Elevator,
            xml_types::DoorEntry::Door(d) => ExitType::Door(DoorHeader {
                destination: RoomId {
                    area: d.toroom.area.into(),
                    room: d.toroom.index.into(),
                },
                transition_flags: d.bitflag,
                transition_type: d.direction,
                doorcap_tile_x: HexU8(d.screenx.0 * 0x10 + d.tilex.0),
                doorcap_tile_y: HexU8(d.screeny.0 * 0x10 + d.tiley.0),
                destination_screen_x: d.screenx,
                destination_screen_y: d.screeny,
                samus_slide_speed: d.distance,
                door_asm: DoorAsmType::from_xml(rom_data, &d.doorcode, exit_name)?,
            }),
        })
    }
}

#[derive(Debug, Hash, Copy, Clone, Serialize)]
struct ScrollDataChange {
    value: HexU8,
    screen_index: HexU8,
}

impl From<&xml_types::ScrollDataChangeEntry> for ScrollDataChange {
    fn from(xml: &xml_types::ScrollDataChangeEntry) -> Self {
        let &xml_types::ScrollDataChangeEntry::Change { screen, scroll } = xml;
        ScrollDataChange {
            value: scroll,
            screen_index: screen,
        }
    }
}

#[derive(Debug, Hash, Serialize)]
enum DoorAsmType {
    Address(HexU16), // label
    ScrollDataUpdate(OwningRef<Vec<ScrollDataChange>>),
    DoorCode(OwningRef<Vec<CodeInstruction>>),
}

#[derive(Debug, Hash, Serialize)]
struct CodeInstruction {
    op: HexU8,
    arg: Option<HexValue>,
}

fn count_true<const N: usize>(conds: [bool; N]) -> usize {
    conds.into_iter().filter(|&b| b).count()
}

impl DoorAsmType {
    fn from_xml(
        rom_data: &mut RomData,
        xml: &xml_types::DoorCode,
        exit_name: &str,
    ) -> Result<DoorAsmType> {
        if 1 != count_true([
            !xml.ops.is_empty(),
            xml.scroll_data.is_some(),
            xml.address.is_some(),
        ]) {
            return Err(anyhow!(
                "Exactly one of `ops`, `scroll_data` or `address` must be set."
            ));
        }

        if let Some(address) = xml.address {
            Ok(DoorAsmType::Address(address))
        } else if let Some(scroll_data) = &xml.scroll_data {
            let scroll_data_update = rom_data.doorcode_scroll_updates.insert(
                scroll_data.entries.iter().map(From::from).collect(),
                format!("DoorASM_Scroll_{exit_name}"),
            );
            Ok(DoorAsmType::ScrollDataUpdate(scroll_data_update))
        } else {
            let doorcode = xml
                .ops
                .iter()
                .map(|x| CodeInstruction {
                    op: x.op,
                    arg: x.arg,
                })
                .collect();
            let doorcode_ref = rom_data
                .doorcode_raw
                .insert(doorcode, format!("DoorASM_Raw_{exit_name}"));
            Ok(DoorAsmType::DoorCode(doorcode_ref))
        }
    }
}

#[derive(Debug, Serialize)]
struct RoomState {
    // Not part of header
    condition_test: HexU16,          // label
    condition_test_args: Vec<HexU8>, // TODO other types and merge with `_test` once that's done

    level_data: OwningRef<Vec<u8>>,

    gfx_set: HexU8,
    music_set: HexU8,
    music_track: HexU8,

    fx_header: Option<OwningRef<FxHeader>>, // Set to $0000 if missing
    enemy_population: OwningRef<EnemyPopulation>,
    enemy_gfx_set: OwningRef<EnemyGfxSet>,

    layer2_scroll_x: HexU8,
    layer2_scroll_y: HexU8,
    layer2_exists: bool, // TODO: Do I need this? Gets OR'd to lower bits of scroll_x/y

    scroll_data: ScrollDataKind,
    unused_roomvar: HexU16,
    main_asm: HexU16, // label
    plm_population: OwningRef<PlmPopulation>,
    bgdata_commands: Option<OwningRef<BgDataCommandList>>,
    setup_asm: HexU16, // label
}

const TILES_PER_SCREEN: usize = 16 * 16;

fn copy_layer_tiles<T, U>(
    src_layer: &xml_types::LevelDataLayer<T>,
    width: usize,
    height: usize,
    dst_data: &mut [U],
    f: impl Fn(&T, &mut U),
) -> Result<()> {
    for screen in &src_layer.screens {
        if screen.x.0 as usize >= width || screen.y.0 as usize >= height {
            return Err(anyhow!(
                "Screen ({},{}) is out of bounds",
                screen.x,
                screen.y
            ));
        }
        if screen.data.len() != TILES_PER_SCREEN {
            return Err(anyhow!(
                "Screen ({},{}) is mis-sized (${:X} tiles)",
                screen.x,
                screen.y,
                screen.data.len()
            ));
        }

        let row_stride = width * 16;
        let mut offset = screen.y.0 as usize * 16 * row_stride + screen.x.0 as usize * 16;
        for row in screen.data.chunks_exact(16) {
            for (a, b) in row.iter().zip(&mut dst_data[offset..offset + 16]) {
                f(a, b);
            }
            offset += row_stride;
        }
    }
    Ok(())
}

fn level_data_from_xml(xml: &xml_types::LevelData) -> Result<Vec<u8>> {
    let width = xml.width.0 as usize;
    let height = xml.height.0 as usize;
    let screens = width * xml.height.0 as usize;
    let total_tiles = screens * TILES_PER_SCREEN;

    let layer1_size = total_tiles * 2;
    let bts_size = total_tiles;
    let layer2_size = if xml.layer2.is_some() {
        total_tiles * 2
    } else {
        0
    };

    let mut buffer = vec![0u8; 2 + layer1_size + bts_size + layer2_size];
    buffer[0..2].copy_from_slice(&(layer1_size as u16).to_le_bytes());
    let (layer1_data, bts_data) = buffer[2..].split_at_mut(layer1_size);
    let (bts_data, layer2_data) = bts_data.split_at_mut(bts_size);

    let layer1_data: &mut [u16] = bytemuck::cast_slice_mut(layer1_data);
    copy_layer_tiles(
        &xml.layer1,
        width,
        height,
        layer1_data,
        |HexU16(src), dst| *dst = src.to_le(),
    )?;

    let bts_data: &mut [u8] = bytemuck::cast_slice_mut(bts_data);
    copy_layer_tiles(&xml.bts, width, height, bts_data, |HexU8(src), dst| {
        *dst = src.to_le()
    })?;

    if let Some(layer2) = &xml.layer2 {
        let layer2_data: &mut [u16] = bytemuck::cast_slice_mut(layer2_data);
        copy_layer_tiles(layer2, width, height, layer2_data, |HexU16(src), dst| {
            *dst = src.to_le()
        })?;
    }

    Ok(buffer)
}

impl RoomState {
    fn from_xml(
        rom_data: &mut RomData,
        xml: &xml_types::RoomState,
        state_name: String,
    ) -> Result<RoomState> {
        let level_data_ref = rom_data.compressed_level_data.insert(
            level_data_from_xml(&xml.level_data)?,
            format!("LevelData_{state_name}"),
        );
        let fx_header_ref = FxHeaderEntry::from_xml(rom_data, &xml.fx1s)?.map(|header| {
            rom_data
                .fx_headers
                .insert(header, format!("FXHeader_{state_name}"))
        });
        let enemy_population_ref = rom_data.enemy_populations.insert(
            EnemyPopulation::from_xml(&xml.enemies),
            format!("EnemyPopulations_{state_name}"),
        );
        let enemy_gfx_set_ref = rom_data.enemy_gfx_sets.insert(
            EnemyGfxSetEntry::from_xml(&xml.enemy_types),
            format!("EnemySets_{state_name}"),
        );
        let scroll_data = ScrollDataKind::from_xml(rom_data, &xml.scroll_data, &state_name)?;
        let plm_population = PlmPopulationEntry::from_xml(rom_data, &xml.plms, &state_name)?;
        let plm_population_ref = rom_data
            .plm_populations
            .insert(plm_population, format!("PLMPopulation_{state_name}"));
        let bgdata_commands = BgDataCommand::from_xml(rom_data, &xml.bg_data, &state_name)?;
        let bgdata_commands_ref = if bgdata_commands.is_empty() {
            None
        } else {
            rom_data
                .bgdata_commands
                .insert(bgdata_commands, format!("LibBG_{state_name}"))
                .into()
        };

        let condition_test = match xml.condition {
            xml_types::StateCondition::Default => HexU16(0xE5E6), // `Use_StatePointer_inX`
            xml_types::StateCondition::Short(a) => a,
        };
        // TODO: Proper sizes/types
        let condition_test_args = xml
            .condition_args
            .iter()
            .map(|x| (x.value.0 as u8).into())
            .collect();

        Ok(RoomState {
            condition_test,
            condition_test_args,
            level_data: level_data_ref,
            gfx_set: xml.gfx_set,
            music_set: (xml.music.0 as u8).into(),
            music_track: ((xml.music.0 >> 8) as u8).into(),
            fx_header: fx_header_ref,
            enemy_population: enemy_population_ref,
            enemy_gfx_set: enemy_gfx_set_ref,
            layer2_scroll_x: xml.layer2_xscroll,
            layer2_scroll_y: xml.layer2_yscroll,
            layer2_exists: xml.layer2_type == xml_types::LayerType::Layer2,
            scroll_data,
            unused_roomvar: xml.roomvar,
            main_asm: xml.fx2,
            plm_population: plm_population_ref,
            bgdata_commands: bgdata_commands_ref,
            setup_asm: xml.layer1_2,
        })
    }
}

type FxHeader = Vec<FxHeaderEntry>;

#[derive(Debug, Hash, Serialize)]
struct FxHeaderEntry {
    from_door: Option<DoorId>,
    liquid_y_start: HexU16,
    liquid_y_target: HexU16,
    liquid_y_speed: HexU16,
    liquid_timer: HexU8,
    fx_type: HexU8,
    layer_blend1: HexU8,
    layer_blend2: HexU8,
    liquid_flags: HexU8,
    enabled_palette_anims: HexU8, // bitset
    enabled_tile_anims: HexU8,    // bitset
    palette_blend_index: HexU8,
}

impl FxHeaderEntry {
    fn from_xml(_rom_data: &mut RomData, xml: &[xml_types::Fx1]) -> Result<Option<FxHeader>> {
        if xml.is_empty() {
            return Ok(None);
        }

        xml.iter()
            .enumerate()
            .map(|(i, fx)| {
                let from_door = match (fx.default, fx.roomarea, fx.roomindex, fx.fromdoor) {
                    (true, None, None, None) => None,
                    (false, Some(area), Some(index), Some(door)) => Some((
                        RoomId {
                            area: area.0,
                            room: index.0,
                        },
                        door.0,
                    )),
                    _ => {
                        return Err(anyhow!(
                            "Fx1 `default` and door reference are mutually exclusive"
                        ));
                    }
                };
                if from_door.is_none() != (i == xml.len() - 1) {
                    dbg!(from_door, i, xml.len() - 1);
                    return Err(anyhow!(
                        "Fx1 without `from_door` must (only) occur at end of header"
                    ));
                }

                Ok(FxHeaderEntry {
                    from_door,
                    liquid_y_start: fx.surfacestart,
                    liquid_y_target: fx.surfacenew,
                    liquid_y_speed: fx.surfacespeed,
                    liquid_timer: fx.surfacedelay,
                    fx_type: fx.type_,
                    layer_blend1: fx.transparency1_a,
                    layer_blend2: fx.transparency2_b,
                    liquid_flags: fx.liquidflags_c,
                    enabled_palette_anims: fx.paletteflags,
                    enabled_tile_anims: fx.animationflags,
                    palette_blend_index: fx.paletteblend,
                })
            })
            .collect::<Result<_>>()
            .map(Some)
    }
}

#[derive(Debug, Hash, Serialize)]
struct EnemyPopulation {
    entries: Vec<EnemyPopulationEntry>,
    kills_required: HexU8,
}

#[derive(Debug, Hash, Serialize)]
struct EnemyPopulationEntry {
    enemy_header: HexU16, // label
    pos_x: HexU16,
    pos_y: HexU16,
    init_param: HexU16, // "Tilemap", initial instruction list pointer, though in practice overriden by the AI init
    flags1: HexU16,     // "Special", upper byte used by engine, lower used by enemy(?)
    flags2: HexU16,     // "Graphics"
    param1: HexU16,     // "Speed"
    param2: HexU16,     // "Spaeed2",
}

impl EnemyPopulation {
    fn from_xml(xml: &xml_types::EnemiesList) -> EnemyPopulation {
        EnemyPopulation {
            entries: xml
                .enemy
                .iter()
                .map(|e| EnemyPopulationEntry {
                    enemy_header: e.id,
                    pos_x: e.x,
                    pos_y: e.y,
                    init_param: e.tilemap,
                    flags1: e.special,
                    flags2: e.gfx,
                    param1: e.speed,
                    param2: e.speed2,
                })
                .collect(),
            kills_required: xml.kill_count,
        }
    }
}

type EnemyGfxSet = Vec<EnemyGfxSetEntry>;

#[derive(Debug, Hash, Serialize)]
struct EnemyGfxSetEntry {
    enemy_header: HexU16, // label
    palette_index_and_flags: HexU16,
}

impl EnemyGfxSetEntry {
    fn from_xml(xml: &[xml_types::EnemyType]) -> EnemyGfxSet {
        xml.iter()
            .map(|e| EnemyGfxSetEntry {
                enemy_header: e.gfx,
                palette_index_and_flags: e.palette,
            })
            .collect()
    }
}

type ScrollData = Vec<HexU8>;

#[derive(Debug, Serialize)]
enum ScrollDataKind {
    /// Stores fixed scroll value minus one. (0 = $01, 1 = $02)
    Fixed(HexU16),
    Ref(OwningRef<ScrollData>),
}

impl ScrollDataKind {
    fn from_xml(
        rom_data: &mut RomData,
        xml: &xml_types::ScrollData,
        state_name: &str,
    ) -> Result<ScrollDataKind> {
        match (xml.const_, xml.data.is_empty()) {
            (Some(fixed), true) => Ok(ScrollDataKind::Fixed(fixed)),
            (None, false) => Ok(ScrollDataKind::Ref(
                rom_data
                    .room_scroll_data
                    .insert(xml.data.clone(), format!("RoomScrolls_{state_name}")),
            )),
            _ => Err(anyhow!("ScrollData must be const or data, but not both")),
        }
    }
}

type PlmPopulation = Vec<PlmPopulationEntry>;

#[derive(Debug, Hash, Serialize)]
struct PlmPopulationEntry {
    plm_header: HexU16, // label
    pos_x: HexU8,
    pos_y: HexU8,
    param: PlmParam,
}

#[derive(Debug, Hash, Serialize)]
enum PlmParam {
    Value(HexU16),
    ScrollDataUpdate(OwningRef<Vec<ScrollDataChange>>),
}

impl PlmPopulationEntry {
    fn from_xml(
        rom_data: &mut RomData,
        xml: &[xml_types::Plm],
        state_name: &str,
    ) -> Result<PlmPopulation> {
        xml.iter()
            .enumerate()
            .map(|(i, plm)| {
                let param = match (plm.arg, &plm.scroll_data) {
                    (Some(arg), None) => PlmParam::Value(arg),
                    (None, Some(scroll_data)) => {
                        let changes = scroll_data.entries.iter().map(From::from).collect();
                        PlmParam::ScrollDataUpdate(
                            rom_data
                                .plm_param_scrolldata
                                .insert(changes, format!("RoomPLM_{state_name}_PLM{i}")),
                        )
                    }
                    _ => {
                        return Err(anyhow!(
                            "PlmPopulationEntry must have only one kind of argument"
                        ));
                    }
                };

                Ok(PlmPopulationEntry {
                    plm_header: plm.type_,
                    pos_x: plm.x,
                    pos_y: plm.y,
                    param,
                })
            })
            .collect::<Result<_>>()
    }
}

type BgDataCommandList = Vec<BgDataCommand>;

#[derive(Debug, Hash, Serialize)]
#[serde(tag = "type")]
enum BgDataCommand {
    CopyToVram(CopyCommandParams), // Command $2
    Decompress {
        // Command $4
        source: BgDataSource,
        dest: HexU16, // $7E address
    },
    _ClearBg3,                        // Command $6
    CopyToVramBg3(CopyCommandParams), // Command $8
    ClearBg2,                         // Command $A
    ClearBg2_2,                       // Command $C
    DoorDependentCopyToVram {
        // Command $E
        // SMART doesn't really support this command, it doesn't try to repoint this value
        from_door: HexU16,
        #[serde(flatten)]
        copy_params: CopyCommandParams,
    },
}

#[derive(Debug, Hash, Serialize)]
struct CopyCommandParams {
    source: BgDataSource,
    dest: HexU16, // VRAM address
    size: HexU16,
}

#[derive(Debug, Hash, Serialize)]
enum BgDataSource {
    Label(HexU24),
    Ref(OwningRef<(Vec<u8>, bool)>),
}

fn copy_hexu16_to_u8(src: &[HexU16]) -> Vec<u8> {
    src.iter().flat_map(|HexU16(x)| x.to_le_bytes()).collect()
}

impl BgDataCommand {
    fn from_xml(
        rom_data: &mut RomData,
        xml: &[xml_types::BgDataEntry],
        state_name: &str,
    ) -> Result<Vec<BgDataCommand>> {
        let mut entries = Vec::new();
        for (i, c) in xml.iter().enumerate() {
            let cmd_name = &format!("{state_name}_Cmd{i}");

            let mut to_bg_data_source =
                |data_or_addr: &xml_types::DataOrAddress, compressed: bool| -> BgDataSource {
                    match data_or_addr {
                        xml_types::DataOrAddress::Data(data) => {
                            BgDataSource::Ref(rom_data.bgdata_tile_data.insert(
                                (copy_hexu16_to_u8(data), compressed),
                                format!("LibBG_{cmd_name}_Data"),
                            ))
                        }
                        &xml_types::DataOrAddress::Address(l) => BgDataSource::Label(l),
                    }
                };

            let mut make_copy_params = |source, dest, size| CopyCommandParams {
                source: to_bg_data_source(source, false),
                dest,
                size,
            };

            entries.push(match *c {
                xml_types::BgDataEntry::Copy {
                    ref source,
                    dest,
                    size,
                } => BgDataCommand::CopyToVram(make_copy_params(source, dest, size)),

                xml_types::BgDataEntry::Decomp {
                    ref source,
                    dest,
                    section: _,
                } => BgDataCommand::Decompress {
                    source: to_bg_data_source(source, true),
                    dest,
                },

                xml_types::BgDataEntry::L3Copy {
                    ref source,
                    dest,
                    size,
                } => BgDataCommand::CopyToVramBg3(make_copy_params(source, dest, size)),

                xml_types::BgDataEntry::Clear2 => BgDataCommand::ClearBg2,
                xml_types::BgDataEntry::ClearAll => BgDataCommand::ClearBg2_2,

                xml_types::BgDataEntry::DdbCopy {
                    ddb,
                    ref source,
                    dest,
                    size,
                } => BgDataCommand::DoorDependentCopyToVram {
                    from_door: ddb,
                    copy_params: make_copy_params(source, dest, size),
                },
            });

            if let xml_types::BgDataEntry::Decomp {
                source,
                dest,
                section: Some(section),
            } = c
            {
                let source_size_words = match source {
                    DataOrAddress::Data(v) => v.len() as u16,
                    _ => {
                        return Err(anyhow!(
                            "BgData command can only have Section if it has embedded data"
                        ));
                    }
                };
                let (section_start, section_size) = match section {
                    DecompSection::Gfx => (0x0, 0x4000),
                    DecompSection::Gfx3 => (0x4000, 0x800),
                    DecompSection::Tiles2 => (0x4800, 0x800),
                    DecompSection::Tiles1 => (0x5000, 0x800),
                    DecompSection::Tiles3 => (0x5800, 0x800),
                };
                let mut offset = 0;
                while offset < section_size {
                    entries.push(BgDataCommand::CopyToVram(CopyCommandParams {
                        source: BgDataSource::Label(HexU24(0x7E0000 + dest.0 as u32)),
                        dest: HexU16(section_start + offset),
                        size: HexU16(source_size_words * 2),
                    }));
                    offset += source_size_words;
                }

                if offset != section_size {
                    return Err(anyhow!(
                        "BgData data (size ${:X}) can not repeat evenly in Section (size ${:X})",
                        source_size_words * 2,
                        section_size
                    ));
                }
            }
        }
        Ok(entries)
    }
}

#[derive(Debug, Serialize)]
struct LoadStation {
    room: RoomId,
    from_door: DoorId,
    unused: HexU16, // Bank logs call it "Door BTS" but that seems vestigial at best
    screen_x: HexU16,
    screen_y: HexU16,
    samus_x: HexU16,
    samus_y: HexU16,
}

impl LoadStation {
    fn from_xml(
        _rom_data: &mut RomData,
        xml: &xml_types::SaveRoom,
        room_id: RoomId,
    ) -> Result<(HexU8, LoadStation)> {
        let from_door = (
            RoomId {
                area: xml.indoor.room_area.0,
                room: xml.indoor.room_index.0,
            },
            xml.indoor.door_index.0,
        );
        Ok((
            xml.saveindex,
            LoadStation {
                room: room_id,
                from_door,
                unused: xml.unused[0].unwrap_or(HexU16(0)),
                screen_x: xml.screenx,
                screen_y: xml.screeny,
                samus_x: xml.samusx,
                samus_y: xml.samusy,
            },
        ))
    }
}

#[derive(Default, Debug, Serialize)]
struct RomData {
    rooms: BTreeMap<RoomId, RoomHeader>,
    doors: DataDeduper<ExitType>,
    load_stations_per_area: Vec<Vec<Option<LoadStation>>>,

    compressed_level_data: DataDeduper<Vec<u8>>,
    fx_headers: DataDeduper<FxHeader>,
    enemy_populations: DataDeduper<EnemyPopulation>,
    enemy_gfx_sets: DataDeduper<EnemyGfxSet>,
    room_scroll_data: DataDeduper<ScrollData>,
    plm_populations: DataDeduper<PlmPopulation>,
    plm_param_scrolldata: DataDeduper<Vec<ScrollDataChange>>,
    bgdata_commands: DataDeduper<BgDataCommandList>,
    bgdata_tile_data: DataDeduper<(Vec<u8>, bool)>, // bool true if data is compressed

    doorcode_scroll_updates: DataDeduper<Vec<ScrollDataChange>>,
    doorcode_raw: DataDeduper<Vec<CodeInstruction>>,
}

macro_rules! impl_rom_data_handle {
    ($field:ident: $type:ty) => {
        impl RomDataHandle for OwningRef<$type> {
            type Target = $type;

            fn resolve<'a>(&self, data: &'a RomData) -> Option<&'a Self::Target> {
                data.$field.get(self).map(|e| &e.data)
            }
        }
    };
}

// rooms: BTreeMap<RoomId, RoomHeader>
impl_rom_data_handle!(doors: ExitType);
// load_stations: BTreeMap<(HexU8, HexU8), LoadStation>
impl_rom_data_handle!(compressed_level_data: Vec<u8>);
impl_rom_data_handle!(fx_headers: FxHeader);
impl_rom_data_handle!(enemy_populations: EnemyPopulation);
impl_rom_data_handle!(enemy_gfx_sets: EnemyGfxSet);
impl_rom_data_handle!(room_scroll_data: ScrollData);
impl_rom_data_handle!(plm_populations: PlmPopulation);
impl_rom_data_handle!(plm_param_scrolldata: Vec<ScrollDataChange>);
impl_rom_data_handle!(bgdata_commands: BgDataCommandList);
impl_rom_data_handle!(bgdata_tile_data: (Vec<u8>, bool));
// doorcode_scroll_updates: DataDeduper<Vec<ScrollDataChange>>
impl_rom_data_handle!(doorcode_raw: Vec<CodeInstruction>);

fn read_room_xml(path: PathBuf) -> Result<xml_types::Room> {
    println!("Parsing {}...", path.display());
    let file = BufReader::new(File::open(path)?);
    let parsed = quick_xml::de::from_reader(file)?;
    Ok(parsed)
}

#[derive(Debug)]
struct TemplateInternalState {
    rom_data: Arc<RomData>,
    compression_queue: Mutex<Vec<PathBuf>>,
    out_dir: PathBuf,
}

impl Object for TemplateInternalState {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Plain
    }
}

const INTERNAL_STATE_KEY: &str = "INTERNAL";

fn get_internal_state(state: &State) -> Result<Arc<TemplateInternalState>, minijinja::Error> {
    Ok(state
        .lookup(INTERNAL_STATE_KEY)
        .and_then(|v| v.downcast_object())
        .unwrap())
}

fn write_file_filter(
    state: &State,
    data: Vec<u8>,
    path: String,
    kwargs: Kwargs,
) -> Result<String, minijinja::Error> {
    let internal_state = get_internal_state(state)?;
    let full_path = internal_state.out_dir.join(&path);

    fs::create_dir_all(full_path.parent().unwrap()).map_err(|e| {
        minijinja::Error::new(ErrorKind::WriteFailure, "failed to create directories")
            .with_source(e)
    })?;

    if let Some(true) = kwargs.get("compress")? {
        let compressed_path = {
            let mut s = full_path.clone().into_os_string();
            s.push(".lz5");
            PathBuf::from(s)
        };

        let changed = write_file_if_not_matching(&full_path, &data).map_err(|e| {
            minijinja::Error::new(ErrorKind::WriteFailure, "failed to write file").with_source(e)
        })?;
        if changed || !compressed_path.exists() {
            internal_state
                .compression_queue
                .lock()
                .unwrap()
                .push(full_path);
        }
        Ok(path + ".lz5")
    } else {
        fs::write(full_path, &data).map_err(|e| {
            minijinja::Error::new(ErrorKind::WriteFailure, "failed to write file").with_source(e)
        })?;
        Ok(path)
    }
}

const TEMPLATE_FILE_LIST: &[&str] = &[
    "room_headers.asm",
    "door_headers.asm",
    "load_stations.asm",
    "fx_headers.asm",
    "enemy_populations.asm",
    "enemy_gfx_sets.asm",
    "room_scroll_data.asm",
    "plm_populations.asm",
    "plm_param_scrolldata.asm",
    "bgdata_commands.asm",
    "doorcode_scroll_updates.asm",
    "doorcode_raw.asm",
    "level_data.asm",
    "bgdata_data.asm",
];

fn emit_asm(rom_data_arc: Arc<RomData>, symbols: Arc<SymbolMap>, out_dir: &Path) -> Result<()> {
    let rom_data = rom_data_arc.as_ref();

    let internal_state = Arc::new(TemplateInternalState {
        rom_data: rom_data_arc.clone(),
        compression_queue: Default::default(),
        out_dir: out_dir.to_path_buf(),
    });

    let mut env = Environment::new();
    env.set_loader(minijinja::path_loader("templates"));
    env.set_trim_blocks(true);
    env.set_lstrip_blocks(true);
    env.set_undefined_behavior(UndefinedBehavior::Strict);
    env.add_global(
        INTERNAL_STATE_KEY,
        Value::from_dyn_object(internal_state.clone()),
    );
    env.add_global("symbols", Value::from_dyn_object(symbols.clone()));
    env.add_filter("hex8", |val: u8| HexU8(val).to_string());
    env.add_filter("data_directive", HexValue::data_directive);
    env.add_filter("write_file", write_file_filter);

    let template_context = context!(data => rom_data);
    for &fname in TEMPLATE_FILE_LIST {
        let template = env.get_template(&format!("{fname}.j2"))?;
        let mut f = File::create(out_dir.join(fname))?;
        template.render_to_write(&template_context, &mut f)?;
    }

    // TODO: Parallelize
    for f in internal_state.compression_queue.lock().unwrap().drain(..) {
        println!("Compressing {}...", f.display());
        compress_lz5_file(&f)?;
    }
    Ok(())
}

fn write_file_if_not_matching(path: &Path, data: &[u8]) -> Result<bool, io::Error> {
    let update_needed = if let Ok(existing_data) = fs::read(path) {
        let mut hasher = DefaultHasher::new();
        existing_data.hash(&mut hasher);
        let existing_hash = hasher.finish();

        let mut hasher = DefaultHasher::new();
        data.hash(&mut hasher);
        let new_hash = hasher.finish();

        new_hash != existing_hash
    } else {
        true
    };

    if update_needed {
        fs::write(path, data)?;
    }
    Ok(update_needed)
}

fn compress_lz5_file(path: &Path) -> Result<()> {
    let status = Command::new("./AmoebaCompress.exe")
        .arg("-c") // Compress
        .arg("-f") // Input file
        .arg(path)
        .status()?;
    if status.success() {
        Ok(())
    } else {
        Err(anyhow!("AmoebaCompress returned with status {}", status))
    }
}

fn main() -> Result<()> {
    let mut rooms = BTreeMap::new();

    for entry in glob("../smart_xml/Export/Rooms/*.xml")? {
        let path = match entry {
            Ok(path) => path,
            Err(e) => {
                eprintln!("Failed to read path: {e}");
                continue;
            }
        };

        use heck::ToUpperCamelCase;

        let room_name = path
            .file_stem()
            .unwrap()
            .to_string_lossy()
            .to_upper_camel_case();
        let room = read_room_xml(path)?;

        match rooms.entry((room.area, room.index)) {
            Entry::Vacant(e) => {
                e.insert((room_name, room));
            }
            Entry::Occupied(e) => {
                let (area_index, room_index) = e.key();
                let old_name = &e.get().0;
                panic!(
                    "Duplicate rooms with id ({area_index},{room_index}): \"{old_name}\" and \"{room_name}\""
                )
            }
        }
    }
    println!("Loaded {} rooms.", rooms.len());

    println!("Loading symbol map...");
    let symbols = SymbolMap::from_wla_sym(File::open("../vanilla.sym")?)?;

    let mut rom_data = RomData::default();
    for ((area_index, room_index), (room_name, xml_room)) in rooms {
        println!("Processing room ({area_index},{room_index}) {room_name}...");
        let (room_id, room) = RoomHeader::from_xml(&mut rom_data, &xml_room, room_name)?;
        rom_data.rooms.insert(room_id, room);
    }
    println!("Processing templates...");
    emit_asm(
        Arc::new(rom_data),
        Arc::new(symbols),
        Path::new("../src/converted/"),
    )?;

    Ok(())
}
