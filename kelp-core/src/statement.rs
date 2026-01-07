use std::collections::BTreeMap;

use crate::command::context::CompileContext;
use crate::datapack::HighDatapack;
use crate::expression::{ConstantExpressionKind, Expression};
use minecraft_command_types::command::Command;
use minecraft_command_types::command::data::{
    DataCommand, DataCommandModification, DataCommandModificationMode,
};
use minecraft_command_types::command::execute::{ExecuteIfSubcommand, ExecuteSubcommand};
use minecraft_command_types::nbt_path::{NbtPath, NbtPathNode};
use minecraft_command_types::range::IntegerRange;
use minecraft_command_types::resource_location::ResourceLocation;
use minecraft_command_types::snbt::{SNBT, SNBTString};
use nonempty::nonempty;
use parser_rs::parser_range::ParserRange;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StatementKind {
    MCFunction(ResourceLocation, Box<Statement>),
    Expression(Expression),
    VariableDeclaration(String, String, Expression),
    While(Expression, Box<Statement>),
    Match(Expression, BTreeMap<IntegerRange, Box<Statement>>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    ForIn(bool, bool, String, Expression, Box<Statement>),
    Block(Vec<Statement>),
    // append <expression> <expression>
    AppendData(Expression, Box<Expression>),
    // remove <expression>
    RemoveData(Expression),
}

fn compile_if(
    datapack: &mut HighDatapack,
    caller_ctx: &mut CompileContext,
    mut body_ctx: CompileContext,
    inverted: bool,
    condition: ExecuteIfSubcommand,
) {
    let should_inine = body_ctx.num_commands() <= 5;

    if should_inine {
        for command in body_ctx.commands {
            caller_ctx.add_command(
                datapack,
                Command::Execute(
                    ExecuteSubcommand::If(inverted, condition.clone())
                        .then(ExecuteSubcommand::Run(Box::new(command))),
                ),
            );
        }
    } else {
        let body_paths = datapack.get_unique_function_paths();

        caller_ctx.add_command(
            datapack,
            Command::Execute(ExecuteSubcommand::If(inverted, condition).then(
                ExecuteSubcommand::Run(Box::new(Command::Function(
                    ResourceLocation::new_namespace_paths(
                        datapack.current_namespace_name(),
                        body_paths.clone(),
                    ),
                    None,
                ))),
            )),
        );

        datapack.compile_and_add_to_function(&body_paths, &mut body_ctx);
    }
}

impl StatementKind {
    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) {
        match self {
            StatementKind::MCFunction(id, statement) => {
                datapack.within_namespace(id.namespace(), |datapack| {
                    datapack.push_function_to_current_namespace(id.paths.clone());

                    let mut function_ctx = CompileContext::default();

                    statement.kind.compile(datapack, &mut function_ctx);

                    let function_commands = function_ctx.compile();

                    datapack
                        .current_namespace_mut()
                        .current_function_mut()
                        .add_commands(function_commands);

                    datapack.pop_function_from_current_namespace();
                });
            }
            StatementKind::Expression(expression) => {
                expression.compile_as_statement(datapack, ctx);
            }
            StatementKind::VariableDeclaration(type_, name, value) => match type_.as_str() {
                "score" => {
                    let Some(score) = datapack.new_get_variable_score(&name) else {
                        panic!("Variable {} has already been declared", name)
                    };

                    value.resolve_into_score(datapack, ctx, &score);

                    datapack.declare_variable(
                        &name,
                        ConstantExpressionKind::PlayerScore(score).into_dummy_constant_expression(),
                    );
                }
                "data" => {
                    let (target, path) = datapack.get_variable_data(&name);

                    value.resolve(datapack, ctx).kind.assign_to_data(
                        datapack,
                        ctx,
                        target.clone(),
                        path.clone(),
                    );

                    datapack.declare_variable(
                        &name,
                        ConstantExpressionKind::Data(target, path).into_dummy_constant_expression(),
                    );
                }
                "value" => {
                    let value = value.resolve(datapack, ctx);

                    datapack.declare_variable(&name, value);
                }
                _ => {
                    panic!("Unknown type '{}'", type_);
                }
            },
            StatementKind::While(condition, body) => {
                let mut while_body_ctx = CompileContext::default();

                body.kind.compile(datapack, &mut while_body_ctx);

                let mut condition_ctx = CompileContext::default();
                let (should_be_inverted, condition) = condition
                    .resolve(datapack, &mut condition_ctx)
                    .kind
                    .to_execute_condition(datapack, &mut condition_ctx, false);

                let while_function_paths = datapack.get_unique_function_paths();

                let current_namespace_name = datapack.current_namespace_name().to_string();

                condition_ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::If(should_be_inverted, condition).then(
                        ExecuteSubcommand::Run(Box::new(Command::Function(
                            ResourceLocation::new_namespace_paths(
                                current_namespace_name,
                                while_function_paths.clone(),
                            ),
                            None,
                        ))),
                    )),
                );

                while_body_ctx.extend_context(condition_ctx.clone());
                ctx.extend_context(condition_ctx);

                datapack.compile_and_add_to_function(&while_function_paths, &mut while_body_ctx);
            }
            StatementKind::ForIn(is_reversed, is_string, variable_name, collection, body) => {
                if is_string {
                    let collection = collection.resolve(datapack, ctx);

                    let (unique_data_target, unique_path, name) = datapack.get_unique_data_named();
                    let (unique_data_target_2, unique_path_2) = datapack.get_unique_data();

                    collection.kind.assign_to_data(
                        datapack,
                        ctx,
                        unique_data_target.clone(),
                        unique_path.clone(),
                    );

                    let mut for_body_ctx = CompileContext::default();

                    datapack.start_scope();
                    datapack.declare_variable(
                        &variable_name,
                        ConstantExpressionKind::Data(
                            unique_data_target_2.clone(),
                            unique_path_2.clone(),
                        )
                        .into_dummy_constant_expression(),
                    );
                    for_body_ctx.add_command(
                        datapack,
                        Command::Data(DataCommand::Modify(
                            unique_data_target_2,
                            unique_path_2,
                            DataCommandModificationMode::Set,
                            DataCommandModification::String(
                                unique_data_target.clone(),
                                Some(unique_path.clone()),
                                Some(if is_reversed { -1 } else { 0 }),
                                if is_reversed { None } else { Some(1) },
                            ),
                        )),
                    );
                    body.kind.compile(datapack, &mut for_body_ctx);
                    datapack.end_scope();

                    let current_namespace_name = datapack.current_namespace_name().to_string();

                    let for_function_paths = datapack.get_unique_function_paths();

                    let mut condition_ctx = CompileContext::default();

                    for_body_ctx.add_command(
                        datapack,
                        Command::Data(DataCommand::Modify(
                            unique_data_target.clone(),
                            unique_path.clone(),
                            DataCommandModificationMode::Set,
                            DataCommandModification::String(
                                unique_data_target.clone(),
                                Some(unique_path.clone()),
                                Some(if is_reversed { 0 } else { 1 }),
                                if is_reversed { Some(-1) } else { None },
                            ),
                        )),
                    );

                    let mut map = BTreeMap::new();
                    map.insert(SNBTString(false, name), SNBT::string(""));
                    let unique_path = NbtPath(nonempty![NbtPathNode::RootCompound(map)]);

                    condition_ctx.add_command(
                        datapack,
                        Command::Execute(ExecuteSubcommand::If(
                            is_string,
                            ExecuteIfSubcommand::Data(
                                unique_data_target,
                                unique_path,
                                Some(Box::new(ExecuteSubcommand::Run(Box::new(
                                    Command::Function(
                                        ResourceLocation::new_namespace_paths(
                                            current_namespace_name,
                                            for_function_paths.clone(),
                                        ),
                                        None,
                                    ),
                                )))),
                            ),
                        )),
                    );

                    for_body_ctx.extend_context(condition_ctx.clone());
                    ctx.extend_context(condition_ctx);

                    datapack.compile_and_add_to_function(&for_function_paths, &mut for_body_ctx);
                } else {
                    let collection = collection.resolve(datapack, ctx);

                    let (unique_data_target, unique_path) = datapack.get_unique_data();

                    collection.kind.assign_to_data(
                        datapack,
                        ctx,
                        unique_data_target.clone(),
                        unique_path.clone(),
                    );

                    let unique_path = unique_path.with_node(NbtPathNode::Index(Some(
                        SNBT::Integer(if is_reversed { -1 } else { 0 }),
                    )));

                    let mut for_body_ctx = CompileContext::default();

                    datapack.start_scope();
                    datapack.declare_variable(
                        &variable_name,
                        ConstantExpressionKind::Data(
                            unique_data_target.clone(),
                            unique_path.clone(),
                        )
                        .into_dummy_constant_expression(),
                    );
                    body.kind.compile(datapack, &mut for_body_ctx);
                    datapack.end_scope();

                    let current_namespace_name = datapack.current_namespace_name().to_string();

                    let for_function_paths = datapack.get_unique_function_paths();

                    let mut condition_ctx = CompileContext::default();

                    for_body_ctx.add_command(
                        datapack,
                        Command::Data(DataCommand::Remove(
                            unique_data_target.clone(),
                            unique_path.clone(),
                        )),
                    );

                    condition_ctx.add_command(
                        datapack,
                        Command::Execute(ExecuteSubcommand::If(
                            false,
                            ExecuteIfSubcommand::Data(
                                unique_data_target,
                                unique_path,
                                Some(Box::new(ExecuteSubcommand::Run(Box::new(
                                    Command::Function(
                                        ResourceLocation::new_namespace_paths(
                                            current_namespace_name,
                                            for_function_paths.clone(),
                                        ),
                                        None,
                                    ),
                                )))),
                            ),
                        )),
                    );

                    for_body_ctx.extend_context(condition_ctx.clone());
                    ctx.extend_context(condition_ctx);

                    datapack.compile_and_add_to_function(&for_function_paths, &mut for_body_ctx);
                }
            }
            StatementKind::Block(body) => {
                datapack.start_scope();

                for statement in body {
                    statement.kind.compile(datapack, ctx);
                }

                datapack.end_scope();
            }
            StatementKind::Match(value, cases) => {
                let min = cases.iter().filter_map(|case| case.0.min).min();
                let max = cases.iter().filter_map(|case| case.0.max).max();

                println!("{:?} {:?}", min, max);

                let _score = value
                    .resolve(datapack, ctx)
                    .kind
                    .as_score(datapack, ctx, false);

                let num_cases = cases.len();

                if num_cases > datapack.settings.num_match_cases_to_split {
                    let middle = num_cases / 2;

                    let mut left = BTreeMap::new();
                    let mut right = BTreeMap::new();

                    for (i, (&k, v)) in cases.iter().enumerate() {
                        if i < middle {
                            left.insert(k, v);
                        } else {
                            right.insert(k, v);
                        }
                    }

                    println!("{:?}", left);
                    println!("{:?}", right);
                    todo!()
                } else {
                    println!("{:?}", cases);

                    todo!()
                }
            }
            StatementKind::If(condition, body, else_body) => {
                let mut if_body_ctx = CompileContext::default();
                body.kind.compile(datapack, &mut if_body_ctx);

                let (invert, compiled_condition) = condition
                    .clone()
                    .resolve(datapack, ctx)
                    .kind
                    .to_execute_condition(datapack, ctx, false);

                compile_if(
                    datapack,
                    ctx,
                    if_body_ctx,
                    invert,
                    compiled_condition.clone(),
                );

                if let Some(else_body) = else_body {
                    let mut else_body_ctx = ctx.create_child_ctx();
                    else_body.kind.compile(datapack, &mut else_body_ctx);

                    compile_if(datapack, ctx, else_body_ctx, !invert, compiled_condition);
                }
            }
            StatementKind::AppendData(target, source) => {
                let target = target.resolve(datapack, ctx);
                let source = source.resolve(datapack, ctx);

                let (target, path) = target.kind.as_data(datapack, ctx);
                let (source, source_path) = source.kind.as_data(datapack, ctx);

                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Modify(
                        target,
                        path,
                        DataCommandModificationMode::Append,
                        DataCommandModification::From(source, Some(source_path)),
                    )),
                );
            }
            StatementKind::RemoveData(expression) => {
                let expression = expression.resolve(datapack, ctx);

                let (target, path) = expression.kind.as_data(datapack, ctx);

                ctx.add_command(datapack, Command::Data(DataCommand::Remove(target, path)));
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Statement {
    pub span: ParserRange,
    pub kind: StatementKind,
}

impl Statement {
    pub fn new(span: ParserRange, kind: StatementKind) -> Statement {
        Statement { span, kind }
    }
}
