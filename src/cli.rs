//! CLI definition and entrypoint for `makina-merkletre`.

use std::path::PathBuf;

use clap::Parser;

#[derive(clap::Subcommand, Debug, Clone, PartialEq)]
pub enum Command {
    /// Transpile input file into a makina rootfile
    Transpile,
    /// Check the input file for usage of unverified contracts
    Check {
        /// Render errors as github workflow commands.
        /// Currently only implemented for checks.
        #[arg(long)]
        #[clap(default_value_t = false)]
        github_errors: bool,
    },
    /// Compute and print the root of the intput file
    Root,
}

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Option<Command>,

    /// Path where the rootfile will be written.
    #[arg(short, long)]
    #[clap(default_value = "rootfile.toml")]
    pub output_file: PathBuf,

    /// Path to the top level caliber file to transpile.
    #[arg(short, long, global = true)]
    #[clap(default_value = "caliber.yaml")]
    pub input_file: PathBuf,
}

impl Cli {
    pub fn command(&self) -> Command {
        self.command.clone().unwrap_or(Command::Transpile)
    }
}
