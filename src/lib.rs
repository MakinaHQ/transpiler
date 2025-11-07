pub mod check;
pub mod cli;
pub mod core;
pub mod errors;
pub mod etherscan;
pub mod meta_sol_types;
pub mod types;

use core::parser::positions::parser::PositionParser;
use core::transpiler::get_rootfile_from_positions;
use std::path::PathBuf;

use eyre::eyre;

use crate::check::Check;
use crate::cli::Command;
use crate::errors::Error;
use crate::errors::Result;
use crate::types::Rootfile;

pub async fn run(cli: &cli::Cli) -> Result<()> {
    color_eyre::install().map_err(Error::Other)?;

    if !cli.input_file.is_file() {
        return Err(Error::Io(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("input file {} not found", cli.input_file.display()),
        )));
    }

    let parsed = PositionParser::new(cli.input_file.clone()).parse()?;

    if let Some(Command::Check) = cli.command {
        let check = Check::new(cli.input_file.clone(), parsed, cli.github_errors);
        return check.all_addresses_verified().await.map_err(Into::into);
    }

    let rootfile = get_rootfile_from_positions(parsed.positions)?;

    write_rootfile(&rootfile, &cli.output_file)
}

/// Writes formatted rootfile to path
fn write_rootfile(content: &Rootfile, out: &PathBuf) -> Result<()> {
    if let Some(parent) = out.parent() {
        let _ = std::fs::create_dir_all(parent);
    }

    let text = toml::to_string_pretty(content)?;

    let format_resp = dprint_plugin_toml::format_text(
        // path to cargo.toml - ignored in our case
        &PathBuf::default(),
        &text,
        &dprint_plugin_toml::configuration::ConfigurationBuilder::new().build(),
    )
    .map_err(|err| eyre!("could not format generated rootfile: {}", err.to_string()))?;

    // `dprint_plugin_toml` returns None if the  text is already formatted correctly
    // in that case we just use the original text
    let formatted = match format_resp {
        Some(formatted) => formatted,
        None => text,
    };

    std::fs::write(out, formatted)?;

    println!("✅ Rootfile successfully transpiled to: {}", out.display());

    Ok(())
}
