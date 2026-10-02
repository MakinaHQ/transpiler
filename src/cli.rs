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
    /// Compute and print the root of an input file
    Root {
        /// Path to an existing TOML rootfile.
        #[arg(long)]
        rootfile: Option<PathBuf>,
    },
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

    /// Path to the json token list.
    /// Required if instructions refer to `token_list`.
    #[arg(short, long)]
    pub token_list: Option<PathBuf>,

    /// Path to the json helpers list.
    /// Required if instructions refer to `helpers`.
    #[arg(long)]
    pub helpers: Option<PathBuf>,

    /// Allow positions without accounting instructions for Makina Lite.
    #[arg(long, global = true)]
    #[clap(default_value_t = false)]
    pub lite: bool,
}

impl Cli {
    pub fn command(&self) -> Command {
        self.command.clone().unwrap_or(Command::Transpile)
    }
}

#[cfg(test)]
mod tests {
    use super::{Cli, Command};
    use clap::Parser;
    use std::path::PathBuf;

    #[test]
    fn root_accepts_a_rootfile() {
        let cli =
            Cli::try_parse_from(["transpiler", "root", "--rootfile", "rootfile.toml"]).unwrap();

        assert_eq!(
            cli.command(),
            Command::Root {
                rootfile: Some(PathBuf::from("rootfile.toml"))
            }
        );
    }
}
