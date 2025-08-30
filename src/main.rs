use crate::asm::SymbolMap;
use crate::config::AppConfig;
use crate::hex_types::{HexU8, HexU16, HexU24, HexValue};
use crate::util::IgnoreMutexPoison;
use anyhow::{Context, Result, anyhow};
use indicatif::ProgressBar;
use minijinja::value::{Enumerator, Kwargs, Object, ObjectExt, ObjectRepr, ValueKind};
use minijinja::{Environment, ErrorKind, State, UndefinedBehavior, Value, context, value};
use serde::ser::{SerializeSeq, SerializeStruct};
use serde::{Deserialize, Serialize, Serializer};
use std::collections::{BTreeMap, HashMap};
use std::fmt::{Debug, Formatter};
use std::fmt::{Display, Write};
use std::fs::File;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::io::BufWriter;
use std::marker::PhantomData;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::{Arc, Mutex};
use std::{fs, io};
use tracing::{debug, error, info_span};
use xxhash_rust::xxh3;

mod asm;
mod config;
mod hex_types;
mod ui;
mod util;
mod xml_conversion;
mod xml_types;

#[derive(Deserialize)]
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

impl<T> Object for OwningRef<T> {
    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        match key.as_str()? {
            "label" => Some(Value::from(self.label())),
            _ => None,
        }
    }

    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Str(&["label"])
    }

    fn render(self: &Arc<Self>, f: &mut Formatter) -> std::fmt::Result
    where
        Self: Sized + 'static,
    {
        f.write_str(self.label())
    }
}

#[derive(Clone, Debug)]
struct DeduperEntry<T> {
    data: T,
    labels: Vec<String>,
    refcount: u32,
}

#[derive(Clone, Debug)]
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

impl<T: Clone + Debug + Send + Sync + Serialize + Hash + 'static> Serialize for DataDeduper<T> {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if value::serializing_for_value() {
            return Value::from_object(self.clone()).serialize(serializer);
        }
        let mut seq = serializer.serialize_seq(Some(self.entries().len()))?;
        for e in self.entries() {
            seq.serialize_element(&(&e.labels, &e.data))?;
        }
        seq.end()
    }
}

impl<T: Debug + Send + Sync + Serialize + Hash + 'static> Object for DataDeduper<T> {
    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        let handle = match OwningRef::<T>::deserialize(key) {
            Ok(v) => v,
            Err(e) => {
                error!("Trying to index with invalid key `{key:?}`: {e:?}");
                return None;
            }
        };
        Some(Value::from_serialize(&self.get(&handle)?.data))
    }

    fn enumerate(self: &Arc<Self>) -> Enumerator {
        self.mapped_enumerator(|this| {
            Box::new(
                this.entries()
                    .iter()
                    .map(|e| Value::from_serialize((&e.labels, &e.data))),
            )
        })
    }

    fn enumerator_len(self: &Arc<Self>) -> Option<usize> {
        Some(self.entries().len())
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

fn get_or_insert_default<T: Default>(vec: &mut Vec<T>, i: usize) -> &mut T {
    if i >= vec.len() {
        vec.resize_with(i + 1, Default::default);
    }
    &mut vec[i]
}

// Unlike RoomHeaders, multiple ids can represent the same DoorHeader
#[derive(Copy, Clone, Debug, Hash, Serialize)]
struct DoorId {
    room: RoomId,
    exit: u8,
}

#[derive(Clone, Debug, Hash, Serialize)]
#[serde(tag = "type")]
enum ExitType {
    Elevator,
    Door(DoorHeader),
}

#[derive(Clone, Debug, Hash, Serialize)]
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

#[derive(Clone, Debug, Hash, Serialize)]
enum DoorAsmType {
    Address(HexU16), // label
    ScrollDataUpdate(OwningRef<Vec<ScrollDataChange>>),
    DoorCode(OwningRef<Vec<CodeInstruction>>),
}

#[derive(Clone, Debug, Hash, Serialize)]
struct CodeInstruction {
    op: HexU8,
    arg: Option<HexValue>,
}

fn count_true<const N: usize>(conds: [bool; N]) -> usize {
    conds.into_iter().filter(|&b| b).count()
}

#[derive(Clone, Debug, Serialize)]
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

#[derive(Clone, Debug, Hash, Serialize)]
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

#[derive(Clone, Debug, Hash, Serialize)]
struct EnemyPopulation {
    entries: Vec<EnemyPopulationEntry>,
    kills_required: HexU8,
}

#[derive(Clone, Debug, Hash, Serialize)]
struct EnemyPopulationEntry {
    enemy_header: HexU16, // label
    pos_x: HexU16,
    pos_y: HexU16,
    init_param: HexU16, // "Tilemap", initial instruction list pointer, though in practice overridden by the AI init
    flags1: HexU16,     // "Special", upper byte used by engine, lower used by enemy(?)
    flags2: HexU16,     // "Graphics"
    param1: HexU16,     // "Speed"
    param2: HexU16,     // "Speed2",
}

type EnemyGfxSet = Vec<EnemyGfxSetEntry>;

#[derive(Clone, Debug, Hash, Serialize)]
struct EnemyGfxSetEntry {
    enemy_header: HexU16, // label
    palette_index_and_flags: HexU16,
}

type ScrollData = Vec<HexU8>;

#[derive(Clone, Debug, Serialize)]
enum ScrollDataKind {
    /// Stores fixed scroll value minus one. (0 = $01, 1 = $02)
    Fixed(HexU16),
    Ref(OwningRef<ScrollData>),
}

type PlmPopulation = Vec<PlmPopulationEntry>;

#[derive(Clone, Debug, Hash, Serialize)]
struct PlmPopulationEntry {
    plm_header: HexU16, // label
    pos_x: HexU8,
    pos_y: HexU8,
    param: PlmParam,
}

#[derive(Clone, Debug, Hash, Serialize)]
enum PlmParam {
    Value(HexU16),
    ScrollDataUpdate(OwningRef<Vec<ScrollDataChange>>),
}

type BgDataCommandList = Vec<BgDataCommand>;

#[derive(Clone, Debug, Hash, Serialize)]
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

#[derive(Clone, Debug, Hash, Serialize)]
struct CopyCommandParams {
    source: BgDataSource,
    dest: HexU16, // VRAM address
    size: HexU16,
}

#[derive(Clone, Debug, Hash, Serialize)]
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

#[derive(Debug, Serialize)]
struct MapLabel {
    x: u16,
    y: u16,
    gfx: HexU16,
}

#[derive(Debug, Serialize)]
struct MapIcon {
    x: u16,
    y: u16,
}

#[derive(Debug, Serialize)]
struct AreaMap {
    tilemap: Vec<u16>,
    title_tilemap: Vec<u16>,
    revealed_tiles_bitmask: Vec<u8>,

    area_labels: Vec<MapLabel>,
    boss_icons: Vec<MapIcon>,
    missile_icons: Vec<MapIcon>,
    energy_icons: Vec<MapIcon>,
    map_icons: Vec<MapIcon>,
    save_icons: Vec<MapIcon>,
}

type TilesetId = u8;

#[derive(Debug, Serialize)]
struct Tileset {
    name: String,
    tiles: OwningRef<TileData>,
    tiletable: OwningRef<TileTable>,
    palette: Option<OwningRef<TilesetPalette>>,
}

#[derive(Clone, Debug, Hash, Serialize)]
struct TileData(Vec<u8>);
#[derive(Clone, Debug, Hash, Serialize)]
struct TileTable(Vec<u16>);
#[derive(Clone, Debug, Hash, Serialize)]
struct TilesetPalette(Vec<u16>);

#[derive(Default, Debug, Serialize)]
struct RomData {
    // Room-rooted data
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

    // AreaMap-rooted data
    area_maps: Vec<Option<AreaMap>>,

    // Tileset-rooted data
    cre_tilesets: BTreeMap<TilesetId, Tileset>,
    sce_tilesets: BTreeMap<TilesetId, Tileset>,
    tileset_tiledata: DataDeduper<TileData>,
    tileset_tiletables: DataDeduper<TileTable>,
    tileset_palettes: DataDeduper<TilesetPalette>,
}

#[derive(Debug)]
struct TemplateInternalState {
    compression_queue: Mutex<Vec<String>>,
    metadata: Mutex<Metadata>,
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

    let changed = write_file_if_not_matching(&full_path, &path, &data, &internal_state.metadata)
        .map_err(|e| {
            minijinja::Error::new(ErrorKind::WriteFailure, "failed to write file").with_source(e)
        })?;

    if let Some(true) = kwargs.get("compress")? {
        let compressed_path = path.clone() + ".lz5";
        if changed || !internal_state.out_dir.join(&compressed_path).exists() {
            internal_state
                .compression_queue
                .lock_unpoisoned()
                .push(path);
        }
        Ok(compressed_path)
    } else {
        Ok(path)
    }
}

fn words_as_bytes_filter(words: Vec<u16>) -> Result<Vec<u8>, minijinja::Error> {
    Ok(bytemuck::pod_collect_to_vec(&words))
}

fn join_with_commas<T: Display, E>(
    mut iter: impl Iterator<Item = Result<T, E>>,
) -> Result<String, E> {
    iter.try_fold(String::new(), |mut result, el| {
        if !result.is_empty() {
            result.push(',');
        }
        write!(&mut result, "{}", el?).unwrap();
        Ok(result)
    })
}

fn hex_filter(value: u64, width: Option<usize>) -> String {
    format!("{value:0width$X}", width = width.unwrap_or(0))
}

fn hex8_filter(value: &Value) -> Result<String, minijinja::Error> {
    match value.kind() {
        ValueKind::Number => {
            let num_val = u8::try_from(value.clone())?;
            Ok(HexU8(num_val).to_string())
        }
        ValueKind::Bytes => {
            join_with_commas(value.as_bytes().unwrap().iter().map(|&b| Ok(HexU8(b))))
        }
        ValueKind::Seq | ValueKind::Iterable => {
            join_with_commas(value.try_iter()?.map(|el| u8::try_from(el).map(HexU8)))
        }
        _ => Err(minijinja::Error::new(
            ErrorKind::InvalidOperation,
            "cannot format value as hex8",
        )),
    }
}

fn hex16_filter(value: &Value) -> Result<String, minijinja::Error> {
    match value.kind() {
        ValueKind::Number => {
            let num_val = u16::try_from(value.clone())?;
            Ok(HexU16(num_val).to_string())
        }
        ValueKind::Seq | ValueKind::Iterable => {
            join_with_commas(value.try_iter()?.map(|el| u16::try_from(el).map(HexU16)))
        }
        _ => Err(minijinja::Error::new(
            ErrorKind::InvalidOperation,
            "cannot format value as hex16",
        )),
    }
}

fn hex24_filter(value: &Value) -> Result<String, minijinja::Error> {
    match value.kind() {
        ValueKind::Number => {
            // TODO: Overflow range checking
            let num_val = u32::try_from(value.clone())?;
            Ok(HexU24(num_val).to_string())
        }
        ValueKind::Seq | ValueKind::Iterable => {
            join_with_commas(value.try_iter()?.map(|el| u32::try_from(el).map(HexU24)))
        }
        _ => Err(minijinja::Error::new(
            ErrorKind::InvalidOperation,
            "cannot format value as hex24",
        )),
    }
}

fn emit_asm(config: &AppConfig, rom_data_arc: Arc<RomData>, symbols: Arc<SymbolMap>) -> Result<()> {
    let metadata = load_metadata(&config.output_dir)?.unwrap_or_default();

    let internal_state = Arc::new(TemplateInternalState {
        compression_queue: Default::default(),
        metadata: metadata.into(),
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
    env.add_filter("hex", hex_filter);
    env.add_filter("h8", hex8_filter);
    env.add_filter("h16", hex16_filter);
    env.add_filter("h24", hex24_filter);
    env.add_filter("data_directive", HexValue::data_directive);
    env.add_filter("write_file", write_file_filter);
    env.add_filter("words_as_bytes", words_as_bytes_filter);

    let template_context = context!(data => rom_data_arc.as_ref());
    for glob_entry in glob::glob(
        config
            .templates_dir
            .join("*.j2")
            .to_str()
            .context("input path is not valid UTF-8")?,
    )? {
        let template_path = glob_entry.context("Failed to read template path")?;
        let template_fname = template_path.file_name().expect("path with filename");
        let template_name = template_fname
            .to_str()
            .with_context(|| format!("template {template_fname:?} has non-UTF-8 name"))?;

        let _span = info_span!("generate_templated_asm", template_name).entered();
        let template = env.get_template(template_name)?;

        let fname = template_name.strip_suffix(".j2").expect("`.j2` suffix");
        let output_path = config.output_dir.join(fname);
        debug!("writing assembly");
        let mut f = BufWriter::new(File::create(output_path)?);
        template.render_to_write(&template_context, &mut f)?;
    }

    drop(env);
    let internal_state = Arc::into_inner(internal_state).unwrap();

    // TODO: Parallelize
    let pb =
        ui::add_progress_bar(ProgressBar::no_length()).with_style(ui::STYLE_MSG_PROGRESS.clone());
    for f in {
        let iter = internal_state
            .compression_queue
            .into_inner_unpoisoned()
            .into_iter();
        pb.set_length(iter.len().try_into().unwrap_or_default());
        iter
    } {
        pb.set_message(format!("Compressing {f}..."));
        pb.inc(1);
        debug!(input_file = f, "compressing");
        compress_lz5_file(config, &f)?;
    }
    pb.finish_and_clear();

    let new_metadata = toml::to_string(&internal_state.metadata.into_inner_unpoisoned())?;
    fs::write(config.output_dir.join(METADATA_FILENAME), new_metadata)?;

    Ok(())
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
#[serde(transparent)]
struct FileHash(#[serde(with = "util::serde_u128_as_hex")] u128);

#[derive(Serialize, Deserialize, Default, Debug)]
struct Metadata {
    file_hashes: BTreeMap<String, FileHash>,
}

const METADATA_FILENAME: &str = "metadata.toml";

fn load_metadata(output_path: &Path) -> Result<Option<Metadata>> {
    let metadata_path = output_path.join(METADATA_FILENAME);
    let file_contents = match fs::read_to_string(metadata_path) {
        Ok(contents) => contents,
        Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(None),
        Err(e) => return Err(e).context("Failed to read metadata.toml"),
    };

    toml::from_str(&file_contents).context("Failed to parse metadata.toml")
}

fn write_file_if_not_matching(
    full_path: &Path,
    metadata_path: &str,
    data: &[u8],
    metadata: &Mutex<Metadata>,
) -> Result<bool, io::Error> {
    let hash = FileHash(xxh3::xxh3_128(data));
    let changed = !full_path.exists() || {
        let metadata = metadata.lock_unpoisoned();
        let old_hash = metadata.file_hashes.get(metadata_path);
        old_hash.is_none_or(|h| *h != hash)
    };

    if changed {
        fs::write(full_path, data)?;

        let mut metadata = metadata.lock_unpoisoned();
        if let Some(old_hash) = metadata.file_hashes.get_mut(metadata_path) {
            *old_hash = hash;
        } else {
            metadata.file_hashes.insert(metadata_path.to_owned(), hash);
        }
    }

    Ok(changed)
}

fn compress_lz5_file(config: &AppConfig, path: &str) -> Result<()> {
    let status = Command::new(&config.compressor_path)
        .arg("-c") // Compress
        .arg("-f") // Input file
        .arg(config.output_dir.join(path))
        .status()?;
    if status.success() {
        Ok(())
    } else {
        Err(anyhow!("AmoebaCompress returned with status {}", status))
    }
}

fn configure_tracing() {
    use tracing_subscriber::EnvFilter;
    use tracing_subscriber::filter::LevelFilter;
    use tracing_subscriber::layer::SubscriberExt;
    use tracing_subscriber::util::SubscriberInitExt;

    tracing_subscriber::registry()
        .with(
            tracing_subscriber::fmt::layer()
                .without_time()
                .with_writer(ui::IndicatifStdoutWriter::new),
        )
        .with(
            EnvFilter::builder()
                .with_default_directive(LevelFilter::INFO.into())
                .from_env_lossy(),
        )
        .init();
}

fn main() -> Result<()> {
    let config = config::app_config().run();
    ui::init_console();
    configure_tracing();

    let rooms = xml_types::load_project_rooms(&config.input_dir)?;
    let areas = xml_types::load_project_area_maps(&config.input_dir)?;
    let tilesets = xml_types::load_project_tilesets(&config.input_dir)?;

    let symbols = SymbolMap::from_wla_sym_file(&config.vanilla_symbols_file)?;

    let mut rom_data = RomData::default();
    for ((area_index, room_index), (room_name, xml_room)) in rooms {
        debug!(%area_index, %room_index, room_name, "processing room");
        let (room_id, room) = RoomHeader::from_xml(&mut rom_data, &xml_room, room_name)?;
        rom_data.rooms.insert(room_id, room);
    }
    for (area_index, xml_area) in areas {
        debug!(%area_index, "processing area map");
        let area = AreaMap::from_xml(xml_area)?;
        *get_or_insert_default(&mut rom_data.area_maps, area_index.into()) = Some(area);
    }
    for (tileset_index, xml_tileset) in tilesets.cre {
        debug!(%tileset_index, "processing CRE tileset");
        let cre = Tileset::from_xml(&mut rom_data, xml_tileset, tileset_index, true)?;
        rom_data.cre_tilesets.insert(tileset_index, cre);
    }
    for (tileset_index, xml_tileset) in tilesets.sce {
        debug!(%tileset_index, "processing SCE tileset");
        let sce = Tileset::from_xml(&mut rom_data, xml_tileset, tileset_index, false)?;
        rom_data.sce_tilesets.insert(tileset_index, sce);
    }

    emit_asm(&config, Arc::new(rom_data), Arc::new(symbols))?;

    Ok(())
}
