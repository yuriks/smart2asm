use crate::hex_types::{HexU16, HexU24};
use anyhow::{anyhow, Result};
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::io::{BufRead, BufReader, Read};
use std::sync::Arc;
use minijinja::{value, Error, State, Value};
use minijinja::value::{Object, ViaDeserialize};

pub enum LookupResult<'a, T> {
    Label(&'a str),
    NotFound(T),
}

impl<T: Display> Display for LookupResult<'_, T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            LookupResult::Label(l) => Display::fmt(l, f),
            LookupResult::NotFound(v) => Display::fmt(v, f),
        }
    }
}

impl<T: Object + 'static> From<LookupResult<'_, T>> for Value {
    fn from(value: LookupResult<T>) -> Value {
        match value {
            LookupResult::Label(l) => l.into(),
            LookupResult::NotFound(a) => Value::from_object(a),
        }
    }
}

#[derive(Debug)]
pub struct SymbolMap {
    addr_to_label: HashMap<u32, String>,
}

impl SymbolMap {
    pub fn new(addr_to_label: HashMap<u32, String>) -> Self {
        SymbolMap { addr_to_label }
    }

    pub fn from_wla_sym(f: impl Read) -> Result<SymbolMap> {
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
        loop {
            let Some(full_line) = lines.next() else {
                break;
            };
            let full_line = full_line?;
            // Start of new section
            if full_line.starts_with("[") {
                break;
            }

            // Strip comment and whitespace
            let l: &str = full_line
                .split_once(';')
                .map_or(&full_line, |(l, _comment)| l);
            let l = l.trim_end();
            if l.is_empty() {
                continue;
            }

            let Some((addr_part, label_part)) = l.split_once(' ') else {
                eprintln!("Malformed line, skipping: {full_line}");
                continue;
            };
            let Some((bank_part, word_addr_part)) = addr_part.split_once(':') else {
                eprintln!("Malformed line, skipping: {full_line}");
                continue;
            };
            let bank = u8::from_str_radix(bank_part, 16)?;
            let word_addr = u16::from_str_radix(word_addr_part, 16)?;

            let full_addr = (bank as u32) << 16 | word_addr as u32;

            addr_to_label.insert(full_addr, label_part.to_string());
        }

        Ok(SymbolMap::new(addr_to_label))
    }

    pub fn resolve_label(&self, bank: u8, addr: HexU16) -> LookupResult<HexU16> {
        let long_addr = (bank as u32) << 16 | addr.0 as u32;
        match self.addr_to_label.get(&long_addr) {
            Some(l) => LookupResult::Label(l),
            None => LookupResult::NotFound(addr),
        }
    }

    pub fn resolve_label_long(&self, addr: HexU24) -> LookupResult<HexU24> {
        match self.addr_to_label.get(&addr.0) {
            Some(l) => LookupResult::Label(l),
            None => LookupResult::NotFound(addr),
        }
    }
}

impl Object for SymbolMap {
    fn call_method(self: &Arc<Self>, _state: &State, method: &str, args: &[Value]) -> Result<Value, Error> {
        match method {
            "resolve_label" => {
                let (bank, ViaDeserialize(addr)) = value::from_args(args)?;
                Ok(Value::from(self.resolve_label(bank, addr)))
            },
            "resolve_label_long" => {
                let (ViaDeserialize(addr),) = value::from_args(args)?;
                Ok(Value::from(self.resolve_label_long(addr)))
            },
            _ => Err(Error::from(minijinja::ErrorKind::UnknownMethod)),
        }
    }
}
