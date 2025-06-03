use indicatif::{MultiProgress, ProgressBar, ProgressDrawTarget, ProgressStyle};
use std::io;
use std::io::Write;
use std::sync::LazyLock;

static GLOBAL_MULTIPROGRESS: LazyLock<MultiProgress> =
    LazyLock::new(|| MultiProgress::with_draw_target(ProgressDrawTarget::hidden()));

pub static STYLE_MSG_ITEMS: LazyLock<ProgressStyle> =
    LazyLock::new(|| ProgressStyle::with_template("{msg}... {pos}").unwrap());

pub static STYLE_MSG_PROGRESS: LazyLock<ProgressStyle> = LazyLock::new(|| {
    ProgressStyle::with_template("{bar} [{pos:.bold}/{len:.bold}] {msg}").unwrap()
});

pub fn init_console() {
    GLOBAL_MULTIPROGRESS.set_draw_target(ProgressDrawTarget::stderr());
}

pub fn add_progress_bar(progress: ProgressBar) -> ProgressBar {
    GLOBAL_MULTIPROGRESS.add(progress)
}

#[derive(Default)]
pub struct IndicatifStdoutWriter {
    buffer: Vec<u8>,
}

impl IndicatifStdoutWriter {
    pub fn new() -> Self {
        Default::default()
    }
}

impl Drop for IndicatifStdoutWriter {
    fn drop(&mut self) {
        let _ = self.flush();
    }
}

impl Write for IndicatifStdoutWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.buffer.extend(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        GLOBAL_MULTIPROGRESS.suspend(|| {
            let mut stdout = io::stdout().lock();
            stdout.write_all(&self.buffer)?;
            self.buffer.clear();
            stdout.flush()
        })
    }
}
