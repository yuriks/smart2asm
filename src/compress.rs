use crate::config::{AppConfig, ExternalCompressor};
use crate::ui;
use anyhow::{Context, anyhow};
use indicatif::ProgressBar;
use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::fs;
use std::path::Path;
use std::process::Command;
use tracing::debug;

trait Compressor {
    fn default_filename_suffix(&self) -> &OsStr;
    fn compress(&self, input_file: &Path, output_file: &Path) -> anyhow::Result<()>;
}

//noinspection RsNeedlessLifetimes
fn substitute_placeholders<'t, 'p>(
    template: &'t str,
    mut placeholder_fn: impl FnMut(&str) -> anyhow::Result<&'p OsStr>,
) -> anyhow::Result<Cow<'t, OsStr>> {
    let mut rest = template;
    let mut result: Option<OsString> = None;
    while let Some((before, after)) = rest.split_once("{{") {
        let result = result.get_or_insert_default();
        result.push(before);
        let Some((placeholder, after)) = after.split_once("}}") else {
            return Err(anyhow!("Unclosed placeholder: {after}"));
        };
        result.push(placeholder_fn(placeholder)?);
        rest = after;
    }

    if let Some(mut result) = result {
        result.push(rest);
        Ok(Cow::Owned(result))
    } else {
        Ok(Cow::Borrowed(template.as_ref()))
    }
}

impl Compressor for ExternalCompressor {
    fn default_filename_suffix(&self) -> &OsStr {
        self.compressed_extension.as_ref()
    }

    fn compress(&self, input_file: &Path, output_file: &Path) -> anyhow::Result<()> {
        let mut command = Command::new(&self.path);
        for arg in &self.params {
            let substituted_arg = substitute_placeholders(arg, |placeholder| match placeholder {
                "input" => Ok(input_file.as_os_str()),
                "output" => Ok(output_file.as_os_str()),
                other => Err(anyhow!("Unknown placeholder: {other:?}")),
            })?;
            command.arg(substituted_arg);
        }
        let status = command
            .status()
            .with_context(|| format!("invoking {command:?}"))?;
        if status.success() {
            Ok(())
        } else {
            Err(anyhow!(
                "{} returned with status {}",
                &self.path.display(),
                status
            ))
        }
    }
}

struct Lznint;

impl Compressor for Lznint {
    fn default_filename_suffix(&self) -> &OsStr {
        "lz5".as_ref()
    }

    fn compress(&self, input_file: &Path, output_file: &Path) -> anyhow::Result<()> {
        // TODO Avoid temporary intermediate file?
        let input_data = fs::read(input_file).context("reading input file")?;
        let output_data = lznint::compress(&input_data);
        fs::write(output_file, &output_data).context("writing output file")?;
        Ok(())
    }
}

pub fn process_compression_queue(
    app_config: &AppConfig,
    compression_queue: Vec<String>,
) -> anyhow::Result<()> {
    // TODO: Parallelize
    let pb =
        ui::add_progress_bar(ProgressBar::no_length()).with_style(ui::STYLE_MSG_PROGRESS.clone());
    for f in {
        let iter = compression_queue.into_iter();
        pb.set_length(iter.len().try_into().unwrap_or_default());
        iter
    } {
        pb.set_message(format!("Compressing {f}..."));
        pb.inc(1);
        compress_file(app_config, &f, &app_config.config.default_compressor)?;
    }
    pb.finish_and_clear();
    Ok(())
}

fn get_compressor<'cfg>(app_config: &'cfg AppConfig, compressor: &str) -> Option<&'cfg dyn Compressor> {
    if let Some(c) = app_config.config.external_compressors.get(compressor) {
        return Some(c);
    }
    match compressor {
        "lznint" => Some(&Lznint),
        _ => None,
    }
}

fn compress_file(app_config: &AppConfig, path: &str, compressor: &str) -> anyhow::Result<()> {
    debug!(input_file = path, "compressing");
    let compressor = get_compressor(app_config, compressor).unwrap();
    let input_path = app_config.cmdline.output_dir.join(path);
    let output_path = input_path.with_added_extension(compressor.default_filename_suffix());
    compressor.compress(&input_path, &output_path)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_substitute_placeholders() {
        let t = "no placeholders";
        let res = substitute_placeholders(t, |_| unreachable!()).unwrap();
        assert_eq!(res, Cow::Borrowed(t));

        let t = "foo{{var}}";
        let res = substitute_placeholders(t, |p| {
            assert_eq!(p, "var");
            Ok("x".as_ref())
        })
        .unwrap();
        let res2: &OsStr = &res;
        assert_eq!(res2, "foox");

        let t = "{{var}}foo";
        let res = substitute_placeholders(t, |p| {
            assert_eq!(p, "var");
            Ok("x".as_ref())
        })
        .unwrap();
        let res2: &OsStr = &res;
        assert_eq!(res2, "xfoo");

        let t = "a{{x}}b{{y}}c";
        let res = substitute_placeholders(t, |p| match p {
            "x" => Ok("-foo-".as_ref()),
            "y" => Ok("-bar-".as_ref()),
            _ => unreachable!(),
        })
        .unwrap();
        let res2: &OsStr = &res;
        assert_eq!(res2, "a-foo-b-bar-c");
    }
}
