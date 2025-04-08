use std::io;
use tracing::subscriber;
use tracing_subscriber::{fmt, fmt::time::ChronoLocal, prelude::*};

pub fn setup_tracing() {
    let env = tracing_subscriber::EnvFilter::from_env("BRIM_LOG");

    let console_layer = fmt::Layer::new()
        .with_writer(io::stderr)
        .with_timer(ChronoLocal::new("%Y-%m-%d %H:%M:%S%.3f".into()))
        .with_ansi(std::io::IsTerminal::is_terminal(&io::stderr()))
        .with_line_number(true)
        .with_file(true)
        .with_target(true);

    let subscriber = tracing_subscriber::Registry::default().with(env).with(console_layer);

    subscriber::set_global_default(subscriber).expect("Failed to set global default subscriber");
}
