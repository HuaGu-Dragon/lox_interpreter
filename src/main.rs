use std::fs;
use std::path::PathBuf;

use clap::Parser;
use clap::Subcommand;
use lox_interpreter::Lexer;
use lox_interpreter::lex::SingleTokenError;
use lox_interpreter::lex::StringTerminationError;
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
    Parse { expr: String },
    ParseFile { filename: PathBuf },
    Eval { expr: String },
}

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
                        if let Some(single_token_error) = e.downcast_ref::<SingleTokenError>() {
                            eprintln!(
                                "[line {}] Error: Unexpected character: {}",
                                single_token_error.line(),
                                single_token_error.token
                            );
                            eprintln!("{e:?}");

                            std::process::exit(65);
                        } else if let Some(string_termination_error) =
                            e.downcast_ref::<StringTerminationError>()
                        {
                            eprintln!(
                                "[line {}] Error: Unterminated string",
                                string_termination_error.line()
                            );
                            eprintln!("{e:?}");

                            std::process::exit(65);
                        }
                        return Err(e);
                    }
                };
                println!("{token}");
            }
            println!("EOF  null");
        }
        Commands::Parse { expr } => {
            let parser = lox_interpreter::Parser::new(None, &expr);
            let expr = match parser.parse_expr() {
                Ok(expr) => expr,
                Err(e) => {
                    if let Some(eof) = e.downcast_ref::<lox_interpreter::lex::Eof>() {
                        eprintln!("[line {}] Error: Unexpected end of file", eof.line());
                        eprintln!("{e:?}");

                        std::process::exit(65);
                    };
                    return Err(e);
                }
            };
            println!("{expr}");
        }
        Commands::ParseFile { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading `{}` failed", filename.display()))?;

            for statement in lox_interpreter::Parser::new(filename.to_str(), &file_contents) {
                let statement = match statement {
                    Ok(statement) => statement,
                    Err(e) => {
                        if let Some(eof) = e.downcast_ref::<lox_interpreter::lex::Eof>() {
                            eprintln!("[line {}] Error: Unexpected end of file", eof.line());
                            eprintln!("{e:?}");

                            std::process::exit(65);
                        };
                        return Err(e);
                    }
                };
                println!("{statement}");
            }
        }
        Commands::Eval { expr } => {
            let mut interpreter = lox_interpreter::eval::Interpreter::new(None, &expr);
            let value = match interpreter.eval_expr() {
                Ok(expr) => expr,
                Err(e) => {
                    if let Some(eof) = e.downcast_ref::<lox_interpreter::lex::Eof>() {
                        eprintln!("[line {}] Error: Unexpected end of file", eof.line());
                        eprintln!("{e:?}");

                        std::process::exit(65);
                    };
                    return Err(e);
                }
            };
            println!("{value}");
        }
    }
    Ok(())
}
