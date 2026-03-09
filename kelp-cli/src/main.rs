use ariadne::{Color, Label, Report, ReportKind, Source};
use clap::{Parser as ClapParser, Subcommand};
use kelp_core::compile_context::CompileContext;
use kelp_core::datapack::HighDatapack;
use kelp_core::item::Item;
use kelp_core::semantic_analysis_context::{
    Scope, SemanticAnalysisContext, SemanticAnalysisInfoKind,
};
use kelp_parser::cst::CSTRoot;
use kelp_parser::lower::root::lower_root;
use kelp_parser::parser::{ParseResult, Parser};
use nonempty::nonempty;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};
use yansi::Paint;

#[derive(ClapParser)]
#[command(name = "kelp")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Build {
        #[arg(value_name = "PATH")]
        path: Option<PathBuf>,

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
            path,
            ignore_validation_errors,
        } => {
            handle_run(path, ignore_validation_errors);
        }
        Commands::New { name } => {
            handle_new(&name.replace('-', "_"));
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

            chars.next().map_or_else(String::new, |first| {
                first.to_uppercase().collect::<String>() + chars.as_str()
            })
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

fn handle_run(project_path: Option<PathBuf>, _ignore_validation_errors: bool) {
    let root = project_path.unwrap_or_else(|| std::env::current_dir().unwrap());
    let kelp_toml = root.join("Kelp.toml");
    let main_kelp = root.join("src/main.kelp");

    let (target_path, project_name) = {
        if kelp_toml.exists() {
            if !main_kelp.exists() {
                eprintln!(
                    "{} Kelp.toml found, but {} is missing.",
                    "Error:".red(),
                    main_kelp.display()
                );
                return;
            }

            let name = fs::read_to_string(&kelp_toml)
                .ok()
                .and_then(|content| {
                    content
                        .lines()
                        .find(|l| l.starts_with("name ="))
                        .and_then(|l| l.split('"').nth(1))
                        .map(ToString::to_string)
                })
                .unwrap_or_else(|| "Kelp Project".to_string());

            (main_kelp, name)
        } else {
            eprintln!(
                "{} No Kelp.toml found at {}.",
                "Error:".red(),
                root.display()
            );
            return;
        }
    };

    let input_text = match fs::read_to_string(&target_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!(
                "{} Could not read {}: {}",
                "Error:".red(),
                target_path.display(),
                e
            );
            return;
        }
    };

    let target_path_str = target_path.to_string_lossy();

    let mut parser = Parser::new(&input_text);

    let start_parse = Instant::now();
    let ParseResult { root, errors } = parser.parse();
    let parse_elapsed = start_parse.elapsed();

    #[cfg(debug_assertions)]
    println!("{:#?}", root);

    let root = CSTRoot::cast(root).unwrap();

    let error_input_text = format!("{} ", input_text);

    let parse_succeeded = errors.is_empty();

    for error in errors {
        let span = (target_path_str.as_ref(), error.span.into_range());

        Report::build(ReportKind::Error, span.clone())
            .with_label(
                Label::new(span)
                    .with_message(error.message)
                    .with_color(Color::Red),
            )
            .finish()
            .print((target_path_str.as_ref(), Source::from(&error_input_text)))
            .unwrap();
    }

    let lower_start = Instant::now();
    let items = lower_root(&root);
    let lower_elapsed = lower_start.elapsed();

    let mut semantic_analysis_context = SemanticAnalysisContext {
        max_infos: 10,
        ..Default::default()
    };
    semantic_analysis_context
        .scopes
        .push_front(Scope::default());

    let start_semantic = Instant::now();
    for statement in &items {
        statement.perform_semantic_analysis(&mut semantic_analysis_context, false);
    }
    let semantic_elapsed = start_semantic.elapsed();
    let semantic_analysis_succeeded = semantic_analysis_context.infos.is_empty();

    for info in semantic_analysis_context.infos {
        match info.kind {
            SemanticAnalysisInfoKind::Error(error) => {
                let span = (&target_path_str, info.span.into_range());
                Report::build(ReportKind::Error, span.clone())
                    .with_label(
                        Label::new(span)
                            .with_message(format!("{}", error))
                            .with_color(Color::Red),
                    )
                    .finish()
                    .print((&target_path_str, Source::from(&input_text)))
                    .unwrap();
            }
        }
    }

    let part_1_elapsed = parse_elapsed + lower_elapsed + semantic_elapsed;

    println!("{} Parsed in {:?}", "Done:".cyan(), parse_elapsed.green());
    println!("{} Lowered in {:?}", "Done:".cyan(), lower_elapsed.green());
    println!(
        "{} Semantic analysis finished in {:?}",
        "Done:".cyan(),
        if semantic_analysis_succeeded {
            semantic_elapsed.green()
        } else {
            semantic_elapsed.red()
        }
    );
    println!(
        "{} Part 1 complete in {:?}",
        "Done:".cyan(),
        part_1_elapsed.green()
    );

    if semantic_analysis_succeeded && parse_succeeded {
        process_success(
            part_1_elapsed,
            items,
            &target_path_str,
            &input_text,
            &project_name,
        );
    }
}

fn process_success(
    existing_elapsed: Duration,
    items: Vec<Item>,
    _file_name: &str,
    _source_text: &str,
    project_name: &str,
) {
    let mut datapack = HighDatapack::new(project_name);
    datapack.settings.num_match_cases_to_split = 5;
    datapack.push_namespace("main");
    datapack.push_function_to_current_namespace(nonempty!["main".to_string()]);

    let mut ctx = CompileContext::default();
    let start_compile = Instant::now();
    for item in items {
        item.kind.compile(&mut datapack, &mut ctx);
    }
    let compile_elapsed = start_compile.elapsed();
    println!(
        "{} Code compilation finished in {:?}",
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
        "{} Datapack compilation finished in {:?}",
        "Done:".cyan(),
        gen_elapsed.green()
    );

    let datapack_dir = dirs::home_dir()
        .unwrap()
        .join(".local/share/PandoraLauncher/instances/1.21.11/.minecraft/saves/1_21_11/datapacks/kelp-rs Datapack");

    if datapack_dir.exists() {
        fs::remove_dir_all(&datapack_dir).unwrap();
    }
    regular_datapack
        .write(datapack_dir.as_path())
        .expect("Failed to write datapack");

    println!(
        "\n{} Total processing time: {:?}",
        "Finished:".bold().bright_green(),
        (existing_elapsed + compile_elapsed + gen_elapsed).green()
    );
}
