smart2asm
=========

smart2asm is a tool which reads XML project files from the [SMART] Super Metroid editor and converts them to assembly and binary data (compressing if necessary).

It is intended to enable usage of SMART in fully asm-based workflows, such as with [InsaneFirebat's sm_disassembly][sm_disassembly] or custom repointing tools, including engine data format customizations. Usage with the disassembly requires adjusting its source to remove the built-in game data and include the generated files instead.

If you have some experience with SM hacking and are interested in making a hack using this, please reach out to me (yuriks) in the Metroid Construction discord. I'll try to help however I can! This hasn't really been tested for developing a real hack yet, so workflow adjustments will probably be needed. (Although I've successfully used it to rebuild the vanilla SM game "from scratch" based on a SMART import.) If you're just starting out, or want a streamlined experience, then you should probably stick to more traditional workflows for now.

[SMART]: https://edit-sm.art/
[sm_disassembly]: https://github.com/InsaneFirebat/sm_disassembly

## Installation

For compiling from source, ensure you have Rust and requirements ([install instructions](https://rust-lang.org/tools/install/)), then just run `cargo build`. The resulting binary will be in `target/debug/smart2asm.exe`. (TODO: Provide downloadable binaries)

Compressing data also requires `AmoebaCompress.exe`. Download it from the [SMART website][SMART] above and place it in the same directory as `smart2asm.exe`. It might require installation of a .NET Core runtime, in which case it'll give a download link to it when you try to execute it.

## Usage

The tool works directly with SMART project files, it does not operate on a ROM or require SMART itself.

### Templates

The output is specified by the `.asm.j2` files in the `templates/` directory. Each file is a text template which will be used to generate a corresponding file in the output directory. Templates use MiniJinja (based on the popular Jinja2 template library). See the [MiniJinja documentation](https://docs.rs/minijinja/#learn-more) for info on [syntax](https://docs.rs/minijinja/latest/minijinja/syntax/index.html) and built-in functions. smart2asm defines additional functions for formatting asm and creating binary files. (TODO: Document these, as well as the data structures, in the meantime, use the default templates and source code as a reference.)

The default templates output asar-flavored assembly, but can be edited freely. It's recommended that you copy the default templates to a new directory, where they can then be removed, added and customized as needed.

### Symbols

A symbol file (In WLA-DX format. Use `--symbols=wla` in asar.) is used to map non-SMART-repointed ROM addresses which appear in the project files directly (e.g. room main asm, enemy AI, PLM ids, etc.) back to labels.

Note that these don't need to correspond to the addresses of symbols in your actual ROM: By starting with a symbols file for vanilla SM, and then allocating new stable unique addresses for any new routines you write, you can ensure that pointers reference the correct labels even as things move around in your built ROM, without needing to keep addresses in SMART synchronized and up to date.

If you don't have a symbol file, it's recommended to generate one by building [sm_disassembly] and copying the generated `symbols.sym` file, so that all common vanilla routines are mapped. It's also possible to create one manually, with the format:
```
[labels]
84:EFD3 PLMEntry_MyNewPLM
8F:E9B0 my_custom_room_asm
; etc. (this is a comment)
```
Pointers that don't map to a label (or if a symbol file is not specified) will be emitted directly as raw addresses.

### Invocation

Run `smart2asm --help` for the full list of options. Example invocation:
```
smart2asm -i smart_project/ -s symbols.sym -T templates/ -o generated_asm/
```

- `smart_project/` is your SMART project directory (the directory containing `project.xml`, `Export`, etc.)
- `symbols.sym` is the symbols file used to map addresses to labels, as explained above.
- `templates/` will be scanned for templates with the `.j2` extension, each of which will generate a corresponding file.
- `generated_asm/` is the directory where the generated files will be written to.

The first execution usually takes some time as AmoebaCompress needs to be invoked to compress each data blob in the game. The `metadata.toml` file in the output directory stores hashes of file contents, so that they won't need to be recompressed in following runs unless their content has changed.

## Improvements

The biggest issues with this workflow right now are iteration times: "Save to XML" needs to be done in SMART each time changes are made before rebuilding the rom, and there is no Quickmet to quickly test a specific room or a way to do so directly from the editor. I've thought about ways to add some kind of convenient Quickmet functionality which integrates with SMART, but for now solutions such as asm defines (configurable from the build commandline) to enable a custom Quickmet patch could be used.
