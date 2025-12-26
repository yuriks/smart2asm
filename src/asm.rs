use crate::hex_types::{HexU16, HexU24};
use crate::ui;
use anyhow::{Context, Result, anyhow};
use indicatif::ProgressBar;
use minijinja::value::{Object, ViaDeserialize};
use minijinja::{Error, State, Value, value};
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Read, Seek};
use std::path::Path;
use std::sync::Arc;
use tracing::{info, warn};

#[derive(Debug)]
pub struct SymbolMap {
    addr_to_label: HashMap<u32, String>,
}

impl SymbolMap {
    pub fn from_map(addr_to_label: HashMap<u32, String>) -> Self {
        SymbolMap { addr_to_label }
    }

    #[tracing::instrument]
    pub fn from_wla_sym_file(path: &Path) -> Result<SymbolMap> {
        Self::from_wla_sym(File::open(path)?)
    }

    pub fn from_wla_sym(f: impl Read + Seek) -> Result<SymbolMap> {
        let pb = ui::add_progress_bar(ProgressBar::no_length());
        pb.set_style(ui::STYLE_MSG_ITEMS.clone());
        pb.set_message("Loading symbols");

        let bf = BufReader::new(f);
        let mut lines = bf.lines();

        'block: {
            for l in &mut lines {
                if l?.starts_with("[labels]") {
                    break 'block;
                }
            }
            return Err(anyhow!("Symbol file has no [labels] section"));
        }

        let mut addr_to_label = HashMap::new();
        for full_line in lines {
            let full_line = full_line?;
            // Start of new section
            if full_line.starts_with("[") {
                break;
            }

            // Strip comment and whitespace
            let l = full_line
                .split_once(';')
                .map_or(full_line.as_str(), |(l, _comment)| l)
                .trim_end();
            if l.is_empty() {
                continue;
            }

            let Ok((bank, word_addr, label)) = parse_label_line(l) else {
                warn!(full_line, "malformed line, skipping");
                continue;
            };

            let full_addr = (bank as u32) << 16 | word_addr as u32;
            addr_to_label.insert(full_addr, label.to_string());
            if addr_to_label.len() % 16 == 0 {
                pb.set_position(addr_to_label.len().try_into().unwrap_or(0));
            }
        }

        pb.set_position(addr_to_label.len().try_into().unwrap_or(0));
        pb.finish_and_clear();

        info!("Loaded {} symbols", addr_to_label.len());
        Ok(SymbolMap::from_map(addr_to_label))
    }

    pub fn resolve_label(&self, bank: u8, addr: HexU16) -> Option<&str> {
        let long_addr = (bank as u32) << 16 | addr.0 as u32;
        self.addr_to_label.get(&long_addr).map(String::as_str)
    }

    pub fn resolve_label_long(&self, addr: HexU24) -> Option<&str> {
        self.addr_to_label.get(&addr.0).map(String::as_str)
    }
}

/// Parses a debug label entry in the form: `BB:AAAA label`
fn parse_label_line(l: &str) -> Result<(u8, u16, &str)> {
    let (addr_part, label_part) = l.split_once(' ').context("malformed line")?;
    let (bank_part, word_addr_part) = addr_part.split_once(':').context("malformed line")?;

    let bank = u8::from_str_radix(bank_part, 16)?;
    let word_addr = u16::from_str_radix(word_addr_part, 16)?;

    Ok((bank, word_addr, label_part))
}

impl Object for SymbolMap {
    fn call_method(
        self: &Arc<Self>,
        _state: &State,
        method: &str,
        args: &[Value],
    ) -> Result<Value, Error> {
        match method {
            "resolve_label" => {
                let (bank, ViaDeserialize(addr)) = value::from_args(args)?;
                Ok(match self.resolve_label(bank, addr) {
                    Some(label) => label.into(),
                    None => Value::from_serialize(addr),
                })
            }
            "resolve_label_long" => {
                let (ViaDeserialize(addr),) = value::from_args(args)?;
                Ok(match self.resolve_label_long(addr) {
                    Some(label) => label.into(),
                    None => Value::from_serialize(addr),
                })
            }
            _ => Err(Error::from(minijinja::ErrorKind::UnknownMethod)),
        }
    }
}
