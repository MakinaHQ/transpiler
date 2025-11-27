use clap::Parser;
use transpiler::cli;

#[tokio::main]
async fn main() -> miette::Result<()> {
    let cli = cli::Cli::parse();
    transpiler::run(&cli).await
}
