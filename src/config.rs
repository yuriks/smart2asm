use bpaf::Bpaf;
use std::path::PathBuf;

const VERSION_STRING: &str = concat!(env!("CARGO_BIN_NAME"), " ", env!("CARGO_PKG_VERSION"));

#[derive(Bpaf, Debug, Clone)]
#[bpaf(options, version(VERSION_STRING))]
/// Converts a SMART project into assembly datafiles for use with the SM Disassembly
pub struct AppConfig {
    /// Path to the AmoebaCompress binary
    #[bpaf(
        long("compressor"), short('C'), argument("FILE"),
        fallback("AmoebaCompress".into()), debug_fallback)]
    pub compressor_path: PathBuf,

    /// Input SMART project directory containing "project.xml"
    #[bpaf(long("input"), short, argument("DIR"))]
    pub input_dir: PathBuf,
    /// Symbols file (in WLA-DX format) used to convert addresses to labels
    #[bpaf(long("vanilla-symbols"), short('s'), argument("FILE"))]
    pub vanilla_symbols_file: PathBuf,
    /// Directory to output generated files to
    #[bpaf(long("output"), short, argument("DIR"))]
    pub output_dir: PathBuf,
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn check_option_invariants() {
        app_config().check_invariants(false)
    }
}
