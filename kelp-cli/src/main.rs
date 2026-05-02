use ariadne::{Color, Label, Report, ReportKind, Source};
use clap::{Parser as ClapParser, Subcommand};
use kelp_core::compile_context::CompileContext;
use kelp_core::datapack::Datapack;
use kelp_core::high::semantic_analysis::SemanticAnalysisContext;
use kelp_core::high::semantic_analysis::info::SemanticAnalysisInfoKind;
use kelp_core::low::environment::Environment;
use kelp_core::low::item::Item;
use kelp_parser::cst::CSTRoot;
use kelp_parser::parser::{ParseResult, Parser};
use kelp_parser::root::lower_root;
use nonempty::nonempty;
use serde::Deserialize;
use std::fs::{self};
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

#[derive(Deserialize)]
struct Project {
    name: String,
    #[allow(unused)]
    id: Option<String>,
    description: Option<String>,
    #[allow(unused)]
    version: String,
}

#[derive(Deserialize)]
struct KelpToml {
    project: Project,
}

fn handle_run(project_path: Option<PathBuf>, _ignore_validation_errors: bool) {
    let root = project_path.unwrap_or_else(|| std::env::current_dir().unwrap());
    let kelp_toml_path_buf = root.join("Kelp.toml");
    let kelp_toml_path = kelp_toml_path_buf.to_string_lossy();
    let main_kelp_path_buf = root.join("src/main.kelp");
    let main_kelp_path = main_kelp_path_buf.to_string_lossy();

    let Ok(kelp_toml) = fs::read_to_string(&kelp_toml_path_buf) else {
        eprintln!(
            "{} No Kelp.toml found at {}.",
            "Error:".red(),
            kelp_toml_path
        );

        return;
    };

    if !main_kelp_path_buf.exists() {
        eprintln!(
            "{} Kelp.toml found, but {} is missing.",
            "Error:".red(),
            main_kelp_path
        );

        return;
    }

    let kelp_toml = match toml::from_str::<KelpToml>(&kelp_toml) {
        Ok(kelp_toml) => kelp_toml,
        Err(error) => {
            eprintln!(
                "{} Failed to parse {}: {}",
                "Error:".red(),
                kelp_toml_path,
                error.message()
            );

            return;
        }
    };

    let main_kelp = match fs::read_to_string(&main_kelp_path_buf) {
        Ok(contents) => contents,
        Err(error) => {
            eprintln!(
                "{} Could not read {}: {}",
                "Error:".red(),
                main_kelp_path,
                error
            );
            return;
        }
    };

    let mut parser = Parser::new(&main_kelp);

    let start_parse = Instant::now();
    let ParseResult { root, errors } = parser.parse();
    let parse_elapsed = start_parse.elapsed();

    #[cfg(debug_assertions)]
    println!("{:#?}", root);

    let root = CSTRoot::cast(root).unwrap();

    let error_input_text = format!("{} ", main_kelp);

    let parse_succeeded = errors.is_empty();

    for error in errors {
        let span = (main_kelp_path.as_ref(), error.span.into_range());

        Report::build(ReportKind::Error, span.clone())
            .with_label(
                Label::new(span)
                    .with_message(error.message)
                    .with_color(Color::Red),
            )
            .finish()
            .print((main_kelp_path.as_ref(), Source::from(&error_input_text)))
            .unwrap();
    }

    let mut semantic_analysis_context = SemanticAnalysisContext::new(10);

    let lower_start = Instant::now();
    let items = lower_root(&root, &mut semantic_analysis_context);
    let lower_elapsed = lower_start.elapsed();

    let start_semantic = Instant::now();
    let items = items
        .into_iter()
        .filter_map(|item| item.perform_semantic_analysis(&mut semantic_analysis_context))
        .collect();
    let semantic_elapsed = start_semantic.elapsed();
    let semantic_analysis_succeeded = semantic_analysis_context.infos.is_empty();

    for info in &semantic_analysis_context.infos {
        match &info.kind {
            SemanticAnalysisInfoKind::Error(error) => {
                let span = (&main_kelp_path, info.span.into_range());
                Report::build(ReportKind::Error, span.clone())
                    .with_label(
                        Label::new(span)
                            .with_message(error.display(&semantic_analysis_context.environment))
                            .with_color(Color::Red),
                    )
                    .finish()
                    .print((&main_kelp_path, Source::from(&main_kelp)))
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
            semantic_analysis_context.environment,
            items,
            &main_kelp_path,
            &main_kelp,
            kelp_toml,
        );
    }
}

fn process_success(
    existing_elapsed: Duration,
    environment: Environment,
    items: Vec<Item>,
    _file_name: &str,
    _source_text: &str,
    kelp_toml: KelpToml,
) {
    let project_name = kelp_toml.project.name;
    let project_description = kelp_toml.project.description;

    let mut datapack = Datapack::new(environment, project_name.clone(), project_description);
    datapack.settings.num_match_cases_to_split = 5;
    datapack.push_namespace("main");
    datapack.push_function_to_current_namespace(nonempty!["main".to_string()]);

    let mut ctx = CompileContext::default();
    let start_compile = Instant::now();
    for item in items {
        item.compile(&mut datapack, &mut ctx);
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

    let number_of_commands = datapack.number_of_commands();
    let number_of_functions = datapack.number_of_functions();
    let number_of_commands_per_function = number_of_commands as f32 / number_of_functions as f32;

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
        .join(".local/share/PandoraLauncher/instances/26.1.2/.minecraft/saves/26_1_2/datapacks/")
        .join(project_name);

    if datapack_dir.exists() {
        fs::remove_dir_all(&datapack_dir).unwrap();
    }

    regular_datapack
        .write(datapack_dir.as_path())
        .expect("Failed to write datapack");

    println!(
        "\n{} {:>6} function(s) generated",
        "Done:".bold().green(),
        number_of_functions,
    );
    println!(
        "{} {:>6} command(s) generated",
        "Done:".bold().green(),
        number_of_commands,
    );
    println!(
        "{} {:>6.2} command(s)/function",
        "Done:".bold().green(),
        number_of_commands_per_function,
    );

    println!(
        "\n{} Total processing time: {:?}",
        "Finished:".bold().green(),
        (existing_elapsed + compile_elapsed + gen_elapsed).green()
    );
}
