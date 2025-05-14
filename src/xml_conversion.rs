use crate::hex_types::{HexU8, HexU16, HexU24};
use crate::{
    BgDataCommand, BgDataSource, CodeInstruction, CopyCommandParams, DoorAsmType, DoorHeader,
    EnemyGfxSet, EnemyGfxSetEntry, EnemyPopulation, EnemyPopulationEntry, ExitType, FxHeader,
    FxHeaderEntry, LoadStation, PlmParam, PlmPopulation, PlmPopulationEntry, RomData, RoomHeader,
    RoomId, RoomState, ScrollDataChange, ScrollDataKind, TILES_PER_SCREEN, xml_types,
};
use anyhow::anyhow;

impl RoomHeader {
    pub fn from_xml(
        rom_data: &mut RomData,
        xml_room: &xml_types::Room,
        room_name: String,
    ) -> anyhow::Result<(RoomId, RoomHeader)> {
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
            .collect::<anyhow::Result<_>>()?;
        let states = xml_room
            .states
            .iter()
            .rev() // SMART Saves room states in reverse order
            .enumerate()
            .map(|(i, x)| RoomState::from_xml(rom_data, x, format!("{room_name}_{i}")))
            .collect::<anyhow::Result<_>>()?;
        for xml_save in &xml_room.saves {
            let (save_id, save) = LoadStation::from_xml(rom_data, xml_save, room_id)?;
            let load_station = crate::get_load_station_mut(
                &mut rom_data.load_stations_per_area,
                xml_room.area,
                save_id,
            );
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

impl ExitType {
    fn from_xml(
        rom_data: &mut RomData,
        xml: &xml_types::DoorEntry,
        exit_name: &str,
    ) -> anyhow::Result<ExitType> {
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

impl From<&xml_types::ScrollDataChangeEntry> for ScrollDataChange {
    fn from(xml: &xml_types::ScrollDataChangeEntry) -> Self {
        let &xml_types::ScrollDataChangeEntry::Change { screen, scroll } = xml;
        ScrollDataChange {
            value: scroll,
            screen_index: screen,
        }
    }
}

impl DoorAsmType {
    fn from_xml(
        rom_data: &mut RomData,
        xml: &xml_types::DoorCode,
        exit_name: &str,
    ) -> anyhow::Result<DoorAsmType> {
        if 1 != crate::count_true([
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

fn copy_layer_tiles<T, U>(
    src_layer: &xml_types::LevelDataLayer<T>,
    width: usize,
    height: usize,
    dst_data: &mut [U],
    f: impl Fn(&T, &mut U),
) -> anyhow::Result<()> {
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

fn level_data_from_xml(xml: &xml_types::LevelData) -> anyhow::Result<Vec<u8>> {
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
    ) -> anyhow::Result<RoomState> {
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

impl FxHeaderEntry {
    fn from_xml(
        _rom_data: &mut RomData,
        xml: &[xml_types::Fx1],
    ) -> anyhow::Result<Option<FxHeader>> {
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
            .collect::<anyhow::Result<_>>()
            .map(Some)
    }
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

impl ScrollDataKind {
    fn from_xml(
        rom_data: &mut RomData,
        xml: &xml_types::ScrollData,
        state_name: &str,
    ) -> anyhow::Result<ScrollDataKind> {
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

impl PlmPopulationEntry {
    fn from_xml(
        rom_data: &mut RomData,
        xml: &[xml_types::Plm],
        state_name: &str,
    ) -> anyhow::Result<PlmPopulation> {
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
            .collect::<anyhow::Result<_>>()
    }
}

impl BgDataCommand {
    fn from_xml(
        rom_data: &mut RomData,
        xml: &[xml_types::BgDataEntry],
        state_name: &str,
    ) -> anyhow::Result<Vec<BgDataCommand>> {
        let mut entries = Vec::new();
        for (i, c) in xml.iter().enumerate() {
            let cmd_name = &format!("{state_name}_Cmd{i}");

            let mut to_bg_data_source =
                |data_or_addr: &xml_types::DataOrAddress, compressed: bool| -> BgDataSource {
                    match data_or_addr {
                        xml_types::DataOrAddress::Data(data) => {
                            BgDataSource::Ref(rom_data.bgdata_tile_data.insert(
                                (crate::copy_hexu16_to_u8(data), compressed),
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
                    xml_types::DataOrAddress::Data(v) => v.len() as u16,
                    _ => {
                        return Err(anyhow!(
                            "BgData command can only have Section if it has embedded data"
                        ));
                    }
                };
                let (section_start, section_size) = match section {
                    xml_types::DecompSection::Gfx => (0x0, 0x4000),
                    xml_types::DecompSection::Gfx3 => (0x4000, 0x800),
                    xml_types::DecompSection::Tiles2 => (0x4800, 0x800),
                    xml_types::DecompSection::Tiles1 => (0x5000, 0x800),
                    xml_types::DecompSection::Tiles3 => (0x5800, 0x800),
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

impl LoadStation {
    fn from_xml(
        _rom_data: &mut RomData,
        xml: &xml_types::SaveRoom,
        room_id: RoomId,
    ) -> anyhow::Result<(HexU8, LoadStation)> {
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
