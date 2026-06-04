use annotate_snippets::{AnnotationKind, Group, Level, Renderer, Snippet};
use clap::{Parser as ClapParser, Subcommand};
use kelp_core::datapack::Datapack;
use kelp_core::parsed::semantic_analysis::SemanticAnalysisContext;
use kelp_core::parsed::semantic_analysis::info::SemanticAnalysisInfo;
use kelp_core::parsed::semantic_analysis::info::diagnostic::LabelType;
use kelp_core::semantic::environment::SemanticEnvironment;
use kelp_core::semantic::program::SemanticProgram;
use kelp_parser::cst::CSTProgram;
use kelp_parser::extension_traits::LowerableAstNode;
use kelp_parser::lower_context::{LowerContext, LowerInfo};
use kelp_parser::parser::{ParseResult, Parser};
use serde::Deserialize;
use std::fs;
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

fn display_semantic_analysis_infos(
    infos: Vec<SemanticAnalysisInfo>,
    semantic_environment: &SemanticEnvironment,
    main_kelp_path: &str,
    main_kelp: &str,
) -> bool {
    let renderer = Renderer::styled();

    let displayed = !infos.is_empty();

    for info in infos {
        match info {
            SemanticAnalysisInfo::Error(error) => {
                let diagnostic = error.into_diagnostic(semantic_environment);

                let mut snippet = Snippet::source(main_kelp).path(main_kelp_path).fold(true);

                for label in &diagnostic.labels {
                    let kind = match label.type_ {
                        LabelType::Primary => AnnotationKind::Primary,
                        LabelType::Secondary => AnnotationKind::Context,
                    };

                    snippet = snippet.annotation(
                        kind.span(label.span.into_range())
                            .label(label.message.as_ref()),
                    );
                }

                let mut group =
                    Group::with_title(Level::ERROR.primary_title(diagnostic.message.clone()))
                        .element(snippet);

                for note in &diagnostic.notes {
                    group = group.element(Level::NOTE.message(note.clone()));
                }

                if let Some(help) = &diagnostic.help {
                    group = group.element(Level::HELP.message(help.clone()));
                }

                println!("{}", renderer.render(&[group]));
                println!();
            }
        }
    }

    displayed
}

fn display_parse_infos(infos: Vec<LowerInfo>, main_kelp_path: &str, main_kelp: &str) -> bool {
    let renderer = Renderer::styled();

    let displayed = !infos.is_empty();

    for info in infos {
        match info {
            LowerInfo::Error(error) => {
                let diagnostic = error.into_diagnostic();

                let mut snippet = Snippet::source(main_kelp).path(main_kelp_path).fold(true);

                for label in &diagnostic.labels {
                    let kind = match label.type_ {
                        LabelType::Primary => AnnotationKind::Primary,
                        LabelType::Secondary => AnnotationKind::Context,
                    };

                    snippet = snippet.annotation(
                        kind.span(label.span.into_range())
                            .label(label.message.as_ref()),
                    );
                }

                let mut group =
                    Group::with_title(Level::ERROR.primary_title(diagnostic.message.clone()))
                        .element(snippet);

                for note in &diagnostic.notes {
                    group = group.element(Level::NOTE.message(note.clone()));
                }

                if let Some(help) = &diagnostic.help {
                    group = group.element(Level::HELP.message(help.clone()));
                }

                println!("{}", renderer.render(&[group]));
                println!();
            }
        }
    }

    displayed
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

    let root = CSTProgram::cast(root).unwrap();

    let parse_succeeded = errors.is_empty();

    let renderer = Renderer::styled();

    for error in errors {
        let snippet = Snippet::source(&main_kelp)
            .path(&main_kelp_path)
            .fold(true)
            .annotation(AnnotationKind::Primary.span(error.span.into_range()));

        let group = Group::with_title(Level::ERROR.primary_title(error.message)).element(snippet);

        println!("{}", renderer.render(&[group]));
        println!();
    }

    let max_infos = 10;

    let mut lower_context = LowerContext::new(max_infos);

    let lower_start = Instant::now();
    let program = root.lower(&mut lower_context).unwrap();
    let lower_elapsed = lower_start.elapsed();

    let lower_succeeded = !display_parse_infos(lower_context.infos, &main_kelp_path, &main_kelp);

    let start_semantic = Instant::now();

    let display_info = |infos: Vec<SemanticAnalysisInfo>,
                        semantic_environment: &SemanticEnvironment| {
        let semantic_elapsed = start_semantic.elapsed();
        let semantic_analysis_succeeded = !display_semantic_analysis_infos(
            infos,
            semantic_environment,
            &main_kelp_path,
            &main_kelp,
        );

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

        (semantic_analysis_succeeded, part_1_elapsed)
    };

    let mut semantic_analysis_context =
        SemanticAnalysisContext::new(&kelp_toml.project.id, max_infos);

    let Some(program) = program.perform_semantic_analysis(&mut semantic_analysis_context) else {
        display_info(
            semantic_analysis_context.infos,
            &semantic_analysis_context.semantic_environment,
        );

        return;
    };

    let (semantic_analysis_succeeded, part_1_elapsed) = display_info(
        semantic_analysis_context.infos,
        &semantic_analysis_context.semantic_environment,
    );

    if lower_succeeded && semantic_analysis_succeeded && parse_succeeded {
        process_success(
            semantic_analysis_context.semantic_environment,
            program,
            &main_kelp_path,
            &main_kelp,
            part_1_elapsed,
            kelp_toml,
        );
    }
}

fn process_success(
    semantic_environment: SemanticEnvironment,
    program: SemanticProgram,
    _file_name: &str,
    _source_text: &str,
    part_1_elapsed: Duration,
    kelp_toml: KelpToml,
) {
    let project_name = kelp_toml.project.name;
    let project_description = kelp_toml.project.description;

    let mut datapack = Datapack::new(
        semantic_environment,
        project_name.clone(),
        project_description,
    );
    datapack.settings.num_match_cases_to_split = 5;

    let start_compile = Instant::now();
    program.compile(&mut datapack);
    let compile_elapsed = start_compile.elapsed();

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
