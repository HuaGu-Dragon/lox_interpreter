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
// 3.06.08
fn main() -> miette::Result<()> {
    let args = Args::parse();

    match args.command {
        Commands::Tokenize { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading `{}` failed", filename.display()))?;

            for token in Lexer::new(filename.to_str(), &file_contents) {
                let token = match token {
                    Ok(token) => token,
                    Err(e) => {
                        if let Some(single_token_error) =
                            e.downcast_ref::<lox_interpreter::SingleTokenError>()
                        {
                            eprintln!(
                                "[line {}] Error: Unexpected character: {}",
                                single_token_error.line(),
                                single_token_error.token
                            );
                            eprintln!("{e:?}");

                            std::process::exit(65);
                        }
                        return Err(e);
                    }
                };
                println!("// expect: {token}");
            }
            println!("// expect: EOF  null");
        }
    }
    Ok(())
}
// expect: LEFT_PAREN ( null
// expect: RIGHT_PAREN ) null
// expect: LEFT_BRACE { null
// expect: RIGHT_BRACE } null
// expect: SEMICOLON ; null
// expect: COMMA , null
// expect: PLUS + null
// expect: MINUS - null
// expect: STAR * null
// expect: BANG_EQUAL != null
// expect: EQUAL_EQUAL == null
// expect: LESS_EQUAL <= null
// expect: GREATER_EQUAL >= null
// expect: BANG_EQUAL != null
// expect: LESS < null
// expect: GREATER > null
// expect: SLASH / null
// expect: DOT . null
// expect: EOF  null

// expect: LEFT_PAREN ( null
// expect: RIGHT_PAREN ) null
// expect: LEFT_BRACE { null
// expect: RIGHT_BRACE } null
// expect: SEMICOLON ; null
// expect: COMMA , null
// expect: PLUS + null
// expect: MINUS - null
// expect: STAR * null
// expect: BANG_EQUAL != null
// expect: EQUAL_EQUAL == null
// expect: LESS_EQUAL <= null
// expect: GREATER_EQUAL >= null
// expect: BANG_EQUAL != null
// expect: LESS < null
// expect: GREATER > null
// expect: SLASH / null
// expect: DOT . null
// expect: EOF  null
