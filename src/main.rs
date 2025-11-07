use clap::Parser;
use eyre::Result;
use transpiler::cli;

#[tokio::main]
async fn main() -> Result<()> {
    let cli = cli::Cli::parse();

    if let Err(e) = transpiler::run(&cli).await {
        eprintln!("❌ Error: {e}");
        std::process::exit(1);
    }

    Ok(())
}
