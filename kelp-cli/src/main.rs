use ariadne::{Color, Label, Report, ReportKind, Source};
use clap::Parser;
use kelp_core::compile_context::CompileContext;
use kelp_core::datapack::HighDatapack;
use kelp_core::generate_message_error;
use kelp_core::semantic_analysis_context::{
    Scope, SemanticAnalysisContext, SemanticAnalysisInfoKind,
};
use kelp_core::statement::Statement;
use kelp_core::trait_ext::OptionIterExt;
use kelp_parser::file;
use nonempty::nonempty;
use parser_rs::ParseResult;
use parser_rs::fn_parser::FnParser;
use parser_rs::stream::Stream;
use std::cmp::min;
use std::fs;
use std::time::Instant;
use yansi::Paint;

fn process_success(statements: Vec<Statement>, file_name: &str, source_text: &str) {
    let mut semantic_analysis_context = SemanticAnalysisContext {
        max_infos: 10,
        ..Default::default()
    };
    semantic_analysis_context
        .scopes
        .push_front(Scope::default());

    let start_semantic = Instant::now();
    let succeeded = statements
        .iter()
        .map(|statement| statement.perform_semantic_analysis(&mut semantic_analysis_context, false))
        .all_some();
    let semantic_elapsed = start_semantic.elapsed();

    println!(
        "{} Semantic analysis finished in {:?}",
        "Done:".cyan(),
        if succeeded.is_some() {
            semantic_elapsed.green()
        } else {
            semantic_elapsed.red()
        }
    );

    if succeeded.is_none() {
        for info in semantic_analysis_context.infos {
            match info.kind {
                SemanticAnalysisInfoKind::Error(error) => {
                    let span = (file_name, info.span.into_range());

                    Report::build(ReportKind::Error, span.clone())
                        .with_label(
                            Label::new(span)
                                .with_message(format!("{}", error))
                                .with_color(Color::Red),
                        )
                        .finish()
                        .print((file_name, Source::from(source_text)))
                        .unwrap();
                }
            }
        }

        return;
    }

    let mut datapack = HighDatapack::new("Name");

    datapack.settings.num_match_cases_to_split = 5;

    datapack.push_namespace("main");
    datapack.push_function_to_current_namespace(nonempty!["main".to_string()]);

    let mut ctx = CompileContext::default();

    let start_compile = Instant::now();
    for statement in statements {
        statement.kind.compile(&mut datapack, &mut ctx);
    }
    let compile_elapsed = start_compile.elapsed();
    println!(
        "{} Statement compilation finished in {:?}",
        "Done:".cyan(),
        compile_elapsed.green()
    );

    datapack.add_context_to_current_function(&mut ctx);
    datapack.pop_function_from_current_namespace();
    datapack.pop_namespace();

    let start_gen = Instant::now();
    let regular_datapack = datapack.compile();
    let gen_elapsed = start_gen.elapsed();
    println!(
        "{} Code generation finished in {:?}",
        "Done:".cyan(),
        gen_elapsed.green()
    );

    let datapack_dir = dirs::home_dir().unwrap().join(".var/app/org.prismlauncher.PrismLauncher/data/PrismLauncher/instances/1.21.11/minecraft/saves/kelp-rs/datapacks/kelp-rs Datapack");

    let start_io = Instant::now();

    if datapack_dir.exists() {
        fs::remove_dir_all(&datapack_dir).unwrap();
    }

    regular_datapack
        .write(datapack_dir.as_path())
        .expect("Failed to write {:?}");

    let io_elapsed = start_io.elapsed();

    println!(
        "\n{} Total processing time: {:?}",
        "Finished:".bold().bright_green(),
        (semantic_elapsed + compile_elapsed + gen_elapsed + io_elapsed).green()
    );
}

fn process_failure<T>(result: ParseResult<T>, file_name: &str, source_text: &str) {
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

    let Err(ref error) = result.result else {
        return;
    };

    let mut new_span = error.span;
    new_span.end = min(new_span.end, source_text.len());
    new_span.start = min(new_span.start, new_span.end);
    let span = (file_name, new_span.into_range());

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
            println!("{} Parsed in {:?}", "Done:".cyan(), elapsed.green());
            process_success(result, file_name, &input_text);
        }
        (false, true) if options.ignore_validation_errors => {
            let result = result.result.unwrap();
            println!(
                "{}",
                "Ignoring validation errors and compiling anyway".yellow()
            );
            process_success(result, file_name, &input_text);
        }
        (_, _) => {
            println!("{} Parsing failed in {:?}", "Error:".red(), elapsed.red());
            process_failure(result, file_name, &input_text);
        }
    }
}
