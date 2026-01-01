use ariadne::{Color, Label, Report, ReportKind, Source};
use clap::Parser;
use indicatif::{ProgressBar, ProgressStyle};
use kelp_core::command::context::CompileContext;
use kelp_core::datapack::HighDatapack;
use kelp_core::generate_message_error;
use kelp_core::statement::Statement;
use kelp_parser::file;
use nonempty::nonempty;
use parser_rs::{FnParser, Stream};
use std::cmp::min;
use std::env::{self};
use std::fs;
use std::path::PathBuf;
use std::time::Instant;
use yansi::Paint;

fn process_success(statements: Vec<Statement>) {
    let mut datapack = HighDatapack::new("Name");

    datapack.settings.num_match_cases_to_split = 5;

    datapack.push_namespace("main");
    datapack.push_function_to_current_namespace(nonempty!["main".to_string()]);

    let mut ctx = CompileContext::default();

    let bar = ProgressBar::new(statements.len() as u64);
    bar.set_style(
        ProgressStyle::default_bar()
            .template("{msg} [{bar:40.cyan/blue}] {pos}/{len}")
            .unwrap(),
    );
    bar.set_message("Compiling statements");
    for statement in statements {
        statement.kind.compile(&mut datapack, &mut ctx);

        bar.inc(1);
    }
    bar.finish_and_clear();

    datapack.add_context_to_current_function(&mut ctx);
    datapack.pop_function_from_current_namespace();
    datapack.pop_namespace();

    let regular_datapack = datapack.compile();
    let appdata = env::var("APPDATA").expect("APPDATA environment variable not set");

    let datapack_dir = [
        appdata.as_str(),
        ".minecraft",
        "saves",
        "BlockPit",
        "datapacks",
        "Kelp Datapack",
    ]
    .iter()
    .collect::<PathBuf>();

    if datapack_dir.exists() {
        fs::remove_dir_all(&datapack_dir).unwrap();
    }

    regular_datapack
        .write(datapack_dir.as_path())
        .expect("Failed to write {:?}");
}

fn process_failure(input: Stream, just_validation: bool, file_name: &str, source_text: &str) {
    let max_error = &input.max_error;

    let mut new_span = max_error.span;
    new_span.end = min(new_span.end, source_text.len());
    new_span.start = min(new_span.start, new_span.end);
    let span = (file_name, new_span.into_range());

    for validation_error in input.validation_errors {
        let span = (file_name, validation_error.span.into_range());

        Report::build(ReportKind::Error, span.clone())
            .with_label(
                Label::new(span)
                    .with_message(validation_error.message)
                    .with_color(Color::Red),
            )
            .finish()
            .print((file_name, Source::from(source_text)))
            .unwrap();
    }

    if !just_validation {
        Report::build(ReportKind::Error, span.clone())
            .with_label(
                Label::new(span)
                    .with_message(generate_message_error(max_error))
                    .with_color(Color::Red),
            )
            .finish()
            .print((file_name, Source::from(source_text)))
            .unwrap();
    }
}

#[derive(Parser, Debug)]
#[command(name = "kelp-rs", version = "1.0")]
struct Options {
    #[arg(long)]
    file_path: String,

    #[arg(long)]
    ignore_validation_errors: bool,
}

fn main() {
    let options = Options::parse();

    let max_validation_errors = if options.ignore_validation_errors {
        None
    } else {
        Some(10)
    };

    let file_name = "main.kelp";
    let input_text = fs::read_to_string(file_name).unwrap();
    let mut input = Stream::new(&input_text, Some(19), max_validation_errors);
    input.signature_help_enabled = true;

    let now = Instant::now();
    let result = file.parse(&mut input);
    let elapsed = now.elapsed();

    println!("{:?}", input.max_error);
    println!("{:?}", input.signatures);

    match (input.validation_errors.is_empty(), result) {
        (true, Some(result)) => {
            println!("Parsed ({}) in ~{:?}", "success".green(), elapsed.green());

            process_success(result);
        }
        (_, result) => {
            println!("Parsed ({}) in ~{:?}", "failure".red(), elapsed.green());

            let just_validation = result.is_some();

            if options.ignore_validation_errors
                && let Some(result) = result
            {
                println!(
                    "{}",
                    "Ignoring validation errors and compiling anyway".yellow()
                );

                process_success(result);
            } else {
                process_failure(input, just_validation, file_name, &input_text);
            }
        }
    }
}
