use ariadne::{Color, Label, Report, ReportKind, Source};
use clap::Parser;
use indicatif::{ProgressBar, ProgressStyle};
use kelp_core::command::context::CompileContext;
use kelp_core::datapack::HighDatapack;
use kelp_core::generate_message_error;
use kelp_core::statement::Statement;
use kelp_parser::file;
use nonempty::nonempty;
use parser_rs::ParseResult;
use parser_rs::fn_parser::FnParser;
use parser_rs::stream::Stream;
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

fn process_failure<T>(result: ParseResult<T>, file_name: &str, source_text: &str) {
    let Err(ref error) = result.result else {
        return;
    };

    let mut new_span = error.span;
    new_span.end = min(new_span.end, source_text.len());
    new_span.start = min(new_span.start, new_span.end);
    let span = (file_name, new_span.into_range());

    for validation_error in &result.validation_errors {
        let span = (file_name, validation_error.span.into_range());

        Report::build(ReportKind::Error, span.clone())
            .with_label(
                Label::new(span)
                    .with_message(validation_error.message.clone())
                    .with_color(Color::Red),
            )
            .finish()
            .print((file_name, Source::from(source_text)))
            .unwrap();
    }

    if result.failed() {
        Report::build(ReportKind::Error, span.clone())
            .with_label(
                Label::new(span)
                    .with_message(generate_message_error(error))
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

    let file_name = "main.kelp";
    let input_text = fs::read_to_string(file_name).unwrap();
    let mut input = Stream::new(&input_text);

    if !options.ignore_validation_errors {
        input.config.max_validation_errors = 10;
    }

    let now = Instant::now();
    let result = file.parse_fully(input);
    let elapsed = now.elapsed();

    match (result.validation_errors.is_empty(), result.succeeded()) {
        (true, true) => {
            let result = result.result.unwrap();

            println!("Parsed ({}) in ~{:?}", "success".green(), elapsed.green());

            process_success(result);
        }
        (false, true) if options.ignore_validation_errors => {
            let result = result.result.unwrap();

            println!(
                "{}",
                "Ignoring validation errors and compiling anyway".yellow()
            );

            process_success(result);
        }
        (_, _) => {
            println!("Parsed ({}) in ~{:?}", "failure".red(), elapsed.green());

            process_failure(result, file_name, &input_text);
        }
    }
}
