use ariadne::{Color, Label, Report, ReportKind, Source};
use clap::{Parser as ClapParser, Subcommand};
use kelp_core::compile_context::CompileContext;
use kelp_core::datapack::Datapack;
use kelp_core::high::semantic_analysis::SemanticAnalysisContext;
use kelp_core::high::semantic_analysis::info::SemanticAnalysisInfoKind;
use kelp_core::low::environment::Environment;
use kelp_core::low::item::Item;
use kelp_core::trait_ext::CollectOptionAllIterExt;
use kelp_parser::cst::CSTRoot;
use kelp_parser::parser::{ParseResult, Parser};
use kelp_parser::root::lower_root;
use serde::Deserialize;
use std::fs::{self};
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};
use yansi::Paint;

fn to_identifier(input: &str) -> String {
    let mut result = String::new();

    for (i, c) in input.chars().enumerate() {
        let valid = if i == 0 {
            c.is_ascii_alphabetic() || c == '_' || c == '-'
        } else {
            c.is_ascii_alphanumeric() || c == '_' || c == '-'
        };

        if valid {
            result.push(c);
        }
    }

    if result.is_empty()
        || !matches!(
            result.chars().next().unwrap(),
            'A'..='Z' | 'a'..='z' | '_' | '-'
        )
    {
        result.insert(0, '_');
    }

    result
}

fn identifier_to_name(input: &str) -> String {
    input
        .split(['-', '_'])
        .filter(|s| !s.is_empty())
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                Some(first) => {
                    first.to_uppercase().collect::<String>() + &chars.as_str().to_lowercase()
                }
                None => String::new(),
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

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

    let project_id = to_identifier(input);

    let project_name = identifier_to_name(&project_id);

    println!(
        r#"{} Creating project "{}" (id: {})..."#,
        "Info:".green(),
        project_name,
        project_id
    );

    if let Err(e) = fs::create_dir_all(&src) {
        eprintln!("{} Failed to create directories: {}", "Error:".red(), e);
        return;
    }

    let main_kelp_content = format!(
        include_str!("project_template/src/main.kelp"),
        project_id = project_id,
    );
    let kelp_toml_content = format!(
        include_str!("project_template/Kelp.toml"),
        project_name = project_name,
        project_id = project_id,
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
    id: String,
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

    let mut semantic_analysis_context = SemanticAnalysisContext::new(&kelp_toml.project.id, 10);

    let lower_start = Instant::now();
    let items = lower_root(&root, &mut semantic_analysis_context);
    let lower_elapsed = lower_start.elapsed();

    let start_semantic = Instant::now();
    let Some(items) = items
        .into_iter()
        .map(|item| item.perform_semantic_analysis(&mut semantic_analysis_context))
        .collect_option_all()
    else {
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

        return;
    };
    let semantic_elapsed = start_semantic.elapsed();
    let semantic_analysis_succeeded = semantic_analysis_context.infos.is_empty();

    let part_1_elapsed = parse_elapsed + lower_elapsed + semantic_elapsed;

    println!(
        "{}     Parsed kelp code in              {:.2?}",
        "Done:".cyan(),
        parse_elapsed.green()
    );
    println!(
        "{}     Lowered kelp code in             {:.2?}",
        "Done:".cyan(),
        lower_elapsed.green()
    );
    println!(
        "{}     Performed semantic analysis in   {:.2?}",
        "Done:".cyan(),
        if semantic_analysis_succeeded {
            semantic_elapsed.green()
        } else {
            semantic_elapsed.red()
        }
    );
    println!(
        "{}     Part 1 complete in               {:.4?}\n",
        "Done:".cyan(),
        part_1_elapsed.green()
    );

    if semantic_analysis_succeeded && parse_succeeded {
        process_success(
            semantic_analysis_context.environment,
            items,
            &main_kelp_path,
            &main_kelp,
            part_1_elapsed,
            kelp_toml,
        );
    }
}

fn process_success(
    environment: Environment,
    items: Vec<Item>,
    _file_name: &str,
    _source_text: &str,
    part_1_elapsed: Duration,
    kelp_toml: KelpToml,
) {
    let project_name = kelp_toml.project.name;
    let project_description = kelp_toml.project.description;

    let mut datapack = Datapack::new(environment, project_name.clone(), project_description);
    datapack.settings.num_match_cases_to_split = 5;
    datapack.push_namespace("main");
    datapack.push_function_to_current_namespace(vec!["main".to_string()]);

    let mut ctx = CompileContext::default();
    let start_compile = Instant::now();
    for item in items {
        item.compile(&mut datapack, &mut ctx);
    }
    let compile_elapsed = start_compile.elapsed();

    datapack.add_context_to_current_function(&mut ctx);
    datapack.pop_function_from_current_namespace();
    datapack.pop_namespace();

    let number_of_commands = datapack.number_of_commands();
    let number_of_functions = datapack.number_of_functions();
    let number_of_commands_per_function = number_of_commands as f32 / number_of_functions as f32;

    let start_datapack_compile = Instant::now();
    let regular_datapack = datapack.compile();
    let datapack_compile_elapsed = start_datapack_compile.elapsed();

    let part_2_elapsed = compile_elapsed + datapack_compile_elapsed;

    let total_elapsed = part_1_elapsed + part_2_elapsed;

    println!(
        "{}     Compiled kelp code in {:.2?}",
        "Done:".cyan(),
        compile_elapsed.green()
    );
    println!(
        "{}     Compiled datapack in  {:.2?}",
        "Done:".cyan(),
        datapack_compile_elapsed.green()
    );

    println!(
        "{}     Part 2 complete in    {:.4?}\n",
        "Done:".cyan(),
        part_2_elapsed.green()
    );

    println!(
        "{} Finished in {:.4?}",
        "Finished:".bold().green(),
        total_elapsed.green()
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
        "{} {:>6} command(s)  generated",
        "Done:".bold().green(),
        number_of_commands,
    );
    println!(
        "{} {:>6.2} command(s) / function",
        "Done:".bold().green(),
        number_of_commands_per_function,
    );
}
