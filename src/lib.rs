pub mod check;
pub mod cli;
pub mod core;
pub mod errors;
pub mod etherscan;
pub mod helpers_list;
pub mod merkletree;
pub mod meta_sol_types;
pub mod token_list;
pub mod types;

use core::parser::positions::parser::PositionParser;
use core::transpiler::get_rootfile_from_positions;
use std::path::{Path, PathBuf};

use eyre::eyre;
use miette::miette;

use crate::check::Check;
use crate::cli::Command;
use crate::errors::Error;
use crate::errors::Result;
use crate::types::Rootfile;

pub async fn run(cli: &cli::Cli) -> miette::Result<()> {
    match cli.command() {
        Command::Transpile => {
            let (_, rootfile) = parse_input_files(cli)?;
            write_rootfile(&rootfile, &cli.output_file).map_err(|err| miette!("{}", err))
        }
        Command::Check { github_errors } => {
            let (parsed, _) = parse_input_files(cli)?;
            let check = Check::new(cli.input_file.clone(), parsed, github_errors);
            check
                .all_addresses_verified()
                .await
                .map_err(|err| miette!("{}", err))
        }
        Command::Root { rootfile } => {
            let rootfile = if let Some(path) = rootfile {
                read_rootfile(&path)?
            } else {
                parse_input_files(cli)?.1
            };
            println!("{}", rootfile.root());
            Ok(())
        }
    }
}

fn read_rootfile(path: &Path) -> miette::Result<Rootfile> {
    let content = std::fs::read_to_string(path)
        .map_err(|err| miette!("could not read rootfile {}: {}", path.display(), err))?;
    content
        .parse()
        .map_err(|err| miette!("could not parse rootfile {}: {}", path.display(), err))
}

/// Parse and validate input files, returning the parsed positions and rootfile.
fn parse_input_files(
    cli: &cli::Cli,
) -> miette::Result<(core::parser::positions::types::Root, Rootfile)> {
    if !cli.input_file.is_file() {
        return Err(Error::Io(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("input file {} not found", cli.input_file.display()),
        )))
        .map_err(|err| miette!("{}", err));
    }

    if let Some(token_list_path) = &cli.token_list
        && !token_list_path.is_file()
    {
        return Err(Error::Io(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("token list file {} not found", token_list_path.display()),
        )))
        .map_err(|err| miette!("{}", err));
    }

    // verify the helpers list path exists, if provided
    if let Some(helpers_path) = &cli.helpers
        && !helpers_path.is_file()
    {
        return Err(Error::Io(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("helpers list file {} not found", helpers_path.display()),
        )))
        .map_err(|err| miette!("{}", err));
    }

    let parsed = PositionParser::new(
        cli.input_file.clone(),
        cli.token_list.clone(),
        cli.helpers.clone(),
    )?
    .with_makina_lite(cli.lite)
    .parse()
    .map_err(|err| miette!("{:?}", err))?;

    let rootfile = get_rootfile_from_positions(&parsed.positions, &parsed.tokens)
        .map_err(|err| miette!("{:?}", err))?;

    Ok((parsed, rootfile))
}

/// Writes formatted rootfile to path
fn write_rootfile(content: &Rootfile, out: &PathBuf) -> Result<()> {
    if let Some(parent) = out.parent() {
        std::fs::create_dir_all(parent).map_err(|e| {
            eyre!(
                "Failed to create output directory '{}': {}",
                parent.display(),
                e
            )
        })?;
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

#[cfg(test)]
mod tests {
    use std::fs;

    use alloy::primitives::FixedBytes;

    use super::read_rootfile;

    #[test]
    fn rootfile_comment_does_not_determine_root() {
        let path = std::env::temp_dir().join(format!(
            "transpiler-rootfile-test-{}.toml",
            std::process::id()
        ));
        fs::write(
            &path,
            "# root: 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff\n[instructions]\n",
        )
        .unwrap();

        let rootfile = read_rootfile(&path).unwrap();
        fs::remove_file(path).unwrap();

        assert_eq!(rootfile.root(), FixedBytes::<32>::ZERO);
    }
}
