//! CLI definition and entrypoint for `makina-merkletre`.

use std::path::PathBuf;

use clap::Parser;

#[derive(clap::Subcommand, Debug, PartialEq)]
pub enum Command {
    Check,
}

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Option<Command>,

    /// Path to the top level caliber file to transpile.
    #[arg(short, long)]
    pub input_file: PathBuf,

    /// Path where the rootfile will be written.
    #[arg(short, long)]
    #[clap(default_value = "rootfile.toml")]
    pub output_file: PathBuf,

    /// Render errors as github workflow commands.
    /// Currently only implemented for checks.
    #[arg(long)]
    #[clap(default_value_t = false)]
    pub github_errors: bool,

    /// Path to the json token list.
    /// Required if instructions refer to `token_list`.
    #[arg(short, long)]
    pub token_list: Option<PathBuf>,
}
