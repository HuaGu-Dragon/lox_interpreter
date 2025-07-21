use std::fs;
use std::path::PathBuf;

use clap::Parser;
use clap::Subcommand;
use lox_interpreter::Lexer;
use miette::IntoDiagnostic;
use miette::WrapErr;

#[derive(Parser, Debug)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    Tokenize { filename: PathBuf },
}
fn main() -> miette::Result<()> {
    let args = Args::parse();

    match args.command {
        Commands::Tokenize { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading `{}` failed", filename.display()))?;

            for token in Lexer::new(&file_contents) {
                let token = token?;
                println!("{token}");
            }
        }
    }
    Ok(())
}
