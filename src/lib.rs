pub mod check;
pub mod cli;
pub mod core;
pub mod errors;
pub mod etherscan;
pub mod merkletree;
pub mod meta_sol_types;
pub mod types;

use core::parser::positions::parser::PositionParser;
use core::transpiler::get_rootfile_from_positions;
use std::path::PathBuf;

use eyre::eyre;
use miette::miette;

use crate::check::Check;
use crate::cli::Command;
use crate::errors::Error;
use crate::errors::Result;
use crate::types::Rootfile;

pub async fn run(cli: &cli::Cli) -> miette::Result<()> {
    if !cli.input_file.is_file() {
        return Err(Error::Io(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("input file {} not found", cli.input_file.display()),
        )))
        .map_err(|err| miette!("{}", err));
    }

    let parsed = PositionParser::new(cli.input_file.clone())
        .parse()
        .map_err(|err| miette!("{}", err))?;

    let rootfile =
        get_rootfile_from_positions(&parsed.positions).map_err(|err| miette!("{}", err))?;

    match cli.command() {
        Command::Transpile { output_file } => {
            write_rootfile(&rootfile, &output_file).map_err(|err| miette!("{}", err))
        }
        Command::Check { github_errors } => {
            let check = Check::new(cli.input_file.clone(), parsed, github_errors);
            check
                .all_addresses_verified()
                .await
                .map_err(|err| miette!("{}", err))
        }
        Command::Root => {
            println!("calculated root: {}", rootfile.root());
            Ok(())
        }
    }
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

    std::fs::write(
        out,
        format!(
            "# this is a generated file - do not edit manually\n# root: {}\n\n{}",
            content.root(),
            formatted
        ),
    )?;

    println!("✅ Rootfile successfully transpiled to: {}", out.display());

    Ok(())
}
