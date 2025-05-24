use bpaf::{Parser, construct, short};
use std::path::PathBuf;

pub struct AppConfig {
    pub compressor_path: PathBuf,

    /// Path to the SMART project being imported
    pub input_dir: PathBuf,
    pub vanilla_symbols_file: PathBuf,
    pub output_dir: PathBuf,
}

const VERSION_STRING: &str = concat!(env!("CARGO_BIN_NAME"), " ", env!("CARGO_PKG_VERSION"));

pub fn make_option_parser() -> bpaf::OptionParser<AppConfig> {
    let compressor_path = short('C')
        .long("compressor")
        .help("Path to the AmoebaCompress binary")
        .argument::<PathBuf>("FILE")
        .fallback("AmoebaCompress".into())
        .debug_fallback();
    let input_dir = short('i')
        .long("input")
        .help("Input SMART project directory containing \"project.xml\"")
        .argument::<PathBuf>("DIR");
    let vanilla_symbols_file = short('s')
        .long("vanilla-symbols")
        .help("Symbols file (in WLA-DX format) used to convert addresses to labels")
        .argument::<PathBuf>("FILE");
    let output_dir = short('o')
        .long("output")
        .help("Directory to output generated files to")
        .argument::<PathBuf>("DIR");

    let combined = construct!(AppConfig {
        compressor_path,
        input_dir,
        vanilla_symbols_file,
        output_dir,
    });
    combined
        .to_options()
        .descr("Converts a SMART project into assembly datafiles for use with the SM Disassembly")
        .version(VERSION_STRING)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn check_option_invariants() {
        make_option_parser().check_invariants(false)
    }
}
