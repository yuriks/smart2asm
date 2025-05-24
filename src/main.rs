use crate::asm::SymbolMap;
use crate::config::AppConfig;
use crate::hex_types::{HexU8, HexU16, HexU24, HexValue};
use anyhow::{Context, Result, anyhow};
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
mod config;
mod hex_types;
mod xml_conversion;
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

#[derive(Debug, Hash, Copy, Clone, Serialize)]
struct ScrollDataChange {
    value: HexU8,
    screen_index: HexU8,
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

type EnemyGfxSet = Vec<EnemyGfxSetEntry>;

#[derive(Debug, Hash, Serialize)]
struct EnemyGfxSetEntry {
    enemy_header: HexU16, // label
    palette_index_and_flags: HexU16,
}

type ScrollData = Vec<HexU8>;

#[derive(Debug, Serialize)]
enum ScrollDataKind {
    /// Stores fixed scroll value minus one. (0 = $01, 1 = $02)
    Fixed(HexU16),
    Ref(OwningRef<ScrollData>),
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

fn emit_asm(config: &AppConfig, rom_data_arc: Arc<RomData>, symbols: Arc<SymbolMap>) -> Result<()> {
    let rom_data = rom_data_arc.as_ref();

    let internal_state = Arc::new(TemplateInternalState {
        rom_data: rom_data_arc.clone(),
        compression_queue: Default::default(),
        out_dir: config.output_dir.clone(),
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
        let mut f = File::create(config.output_dir.join(fname))?;
        template.render_to_write(&template_context, &mut f)?;
    }

    // TODO: Parallelize
    for f in internal_state.compression_queue.lock().unwrap().drain(..) {
        println!("Compressing {}...", f.display());
        compress_lz5_file(config, &f)?;
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

fn compress_lz5_file(config: &AppConfig, path: &Path) -> Result<()> {
    let status = Command::new(&config.compressor_path)
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

fn load_rooms_from_smart(
    project_path: &Path,
) -> Result<BTreeMap<(HexU8, HexU8), (String, xml_types::Room)>> {
    let mut rooms = BTreeMap::new();

    for entry in glob(
        project_path
            .join("Export/Rooms/*.xml")
            .to_str()
            .context("input path is not valid UTF-8")?,
    )? {
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
    Ok(rooms)
}

fn main() -> Result<()> {
    let config = config::make_option_parser().run();

    let rooms = load_rooms_from_smart(&config.input_dir)?;

    println!("Loading symbol map...");
    let symbols = SymbolMap::from_wla_sym(File::open(&config.vanilla_symbols_file)?)?;

    let mut rom_data = RomData::default();
    for ((area_index, room_index), (room_name, xml_room)) in rooms {
        println!("Processing room ({area_index},{room_index}) {room_name}...");
        let (room_id, room) = RoomHeader::from_xml(&mut rom_data, &xml_room, room_name)?;
        rom_data.rooms.insert(room_id, room);
    }
    println!("Processing templates...");
    emit_asm(&config, Arc::new(rom_data), Arc::new(symbols))?;

    Ok(())
}
