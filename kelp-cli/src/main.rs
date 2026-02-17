use ariadne::{Color, Label, Report, ReportKind, Source};
use clap::{Parser, Subcommand};
use kelp_core::compile_context::CompileContext;
use kelp_core::datapack::HighDatapack;
use kelp_core::generate_message_error;
use kelp_core::semantic_analysis_context::{
    Scope, SemanticAnalysisContext, SemanticAnalysisInfoKind,
};
use kelp_core::statement::Statement;
use kelp_core::trait_ext::OptionUnitIterExt;
use kelp_parser::file;
use nonempty::nonempty;
use parser_rs::fn_parser::FnParser;
use parser_rs::stream::Stream;
use parser_rs::ParseResult;
use std::cmp::min;
use std::fs;
use std::path::Path;
use std::time::{Duration, Instant};
use yansi::Paint;

#[derive(Parser)]
#[command(name = "kelp")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Build {
        #[arg(long)]
        ignore_validation_errors: bool,
    },
    New {
        name: String,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Build {
            ignore_validation_errors,
        } => {
            handle_run(ignore_validation_errors);
        }
        Commands::New { name } => {
            handle_new(&name.replace("-", "_"));
        }
    }
}

fn handle_new(input: &str) {
    let root = Path::new(input);
    let src = root.join("src");

    let project_id = input;

    let project_name = input
        .split(['-', '_'])
        .filter(|word| !word.is_empty())
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
            }
        })
        .collect::<Vec<String>>()
        .join(" ");

    println!(
        "{} Creating project \"{}\" (id: {})...",
        "Info:".green(),
        project_name,
        project_id
    );

    if let Err(e) = fs::create_dir_all(&src) {
        eprintln!("{} Failed to create directories: {}", "Error:".red(), e);
        return;
    }

    let main_kelp_content = format!(
        "mcfn {}:main {{\n    tellraw @a \"Hello, World!\"\n}}\n",
        project_id
    );

    let kelp_toml_content = format!(
        "[project]\nname = \"{}\"\nid = \"{}\"\nversion = \"0.1.0\"\n",
        project_name, project_id
    );

    if let Err(e) = fs::write(src.join("main.kelp"), main_kelp_content) {
        eprintln!("{} Failed to write main.kelp: {}", "Error:".red(), e);

        return;
    }

    if let Err(e) = fs::write(root.join("Kelp.toml"), kelp_toml_content) {
        eprintln!("{} Failed to write Kelp.toml: {}", "Error:".red(), e);

        return;
    }

    println!(
        "{} Created project at {}",
        "Done:".green().bold(),
        root.display()
    );
}

fn handle_run(ignore_validation_errors: bool) {
    let (target_path, project_name) = {
        if Path::new("Kelp.toml").exists() {
            let main_path = "src/main.kelp".to_string();
            if !Path::new(&main_path).exists() {
                eprintln!(
                    "{} Kelp.toml found, but {} is missing.",
                    "Error:".red(),
                    main_path
                );
                return;
            }

            let name = fs::read_to_string("Kelp.toml")
                .ok()
                .and_then(|content| {
                    content
                        .lines()
                        .find(|l| l.starts_with("name ="))
                        .and_then(|l| l.split('"').nth(1))
                        .map(|s| s.to_string())
                })
                .unwrap_or_else(|| "Kelp Project".to_string());

            (main_path, name)
        } else {
            eprintln!("{} No Kelp.toml found.", "Error:".red());
            return;
        }
    };

    let input_text = match fs::read_to_string(&target_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("{} Could not read {}: {}", "Error:".red(), target_path, e);
            return;
        }
    };

    let mut input = Stream::new(&input_text);
    if !ignore_validation_errors {
        input.config.max_validation_errors = 10;
    }

    let start_parse = Instant::now();
    let result = file.parse_fully(input);
    let parse_elapsed = start_parse.elapsed();

    match (result.validation_errors.is_empty(), result.succeeded()) {
        (true, true) => {
            println!("{} Parsed in {:?}", "Done:".cyan(), parse_elapsed.green());
            process_success(
                parse_elapsed,
                result.result.unwrap(),
                &target_path,
                &input_text,
                &project_name,
            );
        }
        (false, true) if ignore_validation_errors => {
            println!(
                "{}",
                "Ignoring validation errors and compiling anyway".yellow()
            );
            process_success(
                parse_elapsed,
                result.result.unwrap(),
                &target_path,
                &input_text,
                &project_name,
            );
        }
        (_, _) => {
            println!(
                "{} Parsing failed in {:?}",
                "Error:".red(),
                parse_elapsed.red()
            );
            process_failure(result, &target_path, &input_text);
        }
    }
}

fn process_success(
    parse_elapsed: Duration,
    statements: Vec<Statement>,
    file_name: &str,
    source_text: &str,
    project_name: &str,
) {
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

    let mut datapack = HighDatapack::new(project_name);
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
        .expect("Failed to write datapack");
    let io_elapsed = start_io.elapsed();

    println!(
        "\n{} Total processing time: {:?}",
        "Finished:".bold().bright_green(),
        (parse_elapsed + semantic_elapsed + compile_elapsed + gen_elapsed + io_elapsed).green()
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
