use bpaf::Bpaf;
use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

const VERSION_STRING: &str = concat!(env!("CARGO_BIN_NAME"), " ", env!("CARGO_PKG_VERSION"));

#[derive(Bpaf, Debug, Clone)]
#[bpaf(options, version(VERSION_STRING))]
/// Converts a SMART project into assembly datafiles for use with the SM Disassembly
pub struct CmdlineOptions {
    /// Path to the AmoebaCompress binary
    #[bpaf(
        long("compressor"), short('C'), argument("FILE"),
        fallback("AmoebaCompress".into()), debug_fallback)]
    pub compressor_path: PathBuf,

    /// Path to additional configuration file
    #[bpaf(long("config"), short, argument("FILE"))]
    pub config_path: Option<PathBuf>,

    /// Input SMART project directory containing "project.xml"
    #[bpaf(long("input"), short, argument("DIR"))]
    pub input_dir: PathBuf,
    /// Symbols file (in WLA-DX format) used to convert addresses to labels
    #[bpaf(long("vanilla-symbols"), short('s'), argument("FILE"))]
    pub vanilla_symbols_file: Option<PathBuf>,
    /// Directory with Jinja2 templates to generate assembly from
    #[bpaf(long("templates"), short('T'), argument("DIR"))]
    pub templates_dir: PathBuf,
    /// Directory to output generated files to
    #[bpaf(long("output"), short, argument("DIR"))]
    pub output_dir: PathBuf,
}

#[derive(Deserialize)]
pub struct ExternalCompressor {
    pub path: PathBuf,
    pub params: Vec<String>,
    pub compressed_suffix: String,
}

#[derive(Deserialize)]
pub struct ConfigOptions {
    pub default_compressor: String,
    pub external_compressors: HashMap<String, ExternalCompressor>,
}

pub struct AppConfig {
    pub cmdline: CmdlineOptions,
    pub config: ConfigOptions,
}

const DEFAULT_CONFIG: &str = include_str!("../default_config.toml");

pub fn load_config(config_path: Option<&Path>) -> anyhow::Result<ConfigOptions> {
    if let Some(config_path) = config_path {
        Ok(toml::from_str(&fs::read_to_string(config_path)?)?)
    } else {
        Ok(toml::from_str(DEFAULT_CONFIG).unwrap())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn check_option_invariants() {
        cmdline_options().check_invariants(false)
    }

    #[test]
    fn check_default_config() {
        let _config: ConfigOptions = toml::from_str(DEFAULT_CONFIG).unwrap();
    }
}
