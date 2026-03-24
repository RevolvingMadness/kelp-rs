use std::collections::HashMap;

use crate::compile_context::{LoopInfo, LoopType};
use crate::middle::data_type::DataType;
use crate::middle::item::Item;
use crate::middle::pattern::Pattern;
use crate::{
    compile_context::CompileContext, datapack::Datapack,
    low::expression::Expression as LowExpression, middle::expression::Expression,
};
use minecraft_command_types::command::Command;
use minecraft_command_types::command::data::{DataCommand, DataCommandModificationMode};
use minecraft_command_types::command::execute::ExecuteSubcommand;
use minecraft_command_types::command::r#return::ReturnCommand;
use minecraft_command_types::range::IntegerRange;

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Let(DataType, Pattern, Expression),
    Match(Expression, HashMap<IntegerRange, Box<Self>>),
    Append(Expression, Box<Expression>),
    Remove(Expression),
    Item(Box<Item>),
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub struct ControlFlow {
    pub kind: ControlFlowKind,
    pub loop_info: LoopInfo,
}

#[derive(Debug, Clone, Copy)]
pub enum ControlFlowKind {
    Break,
    Continue,
}

impl ControlFlowKind {
    #[must_use]
    pub const fn name(&self) -> &str {
        match self {
            Self::Break => "break",
            Self::Continue => "continue",
        }
    }
}

impl Statement {
    #[must_use]
    pub fn get_control_flow_kind(&self) -> Option<ControlFlowKind> {
        match self {
            Self::Expression(expression) => expression.kind.get_control_flow_kind(),

            Self::Break => Some(ControlFlowKind::Break),
            Self::Continue => Some(ControlFlowKind::Continue),

            _ => None,
        }
    }

    #[must_use]
    pub fn get_data_type(&self) -> Option<DataType> {
        match self {
            Self::Expression(expression) => Some(expression.data_type.clone()),
            Self::Let(_, _, _)
            | Self::Match(_, _)
            | Self::Append(_, _)
            | Self::Remove(_)
            | Self::Item(_)
            | Self::Break
            | Self::Continue => None,
        }
    }

    pub fn compile_as_statement(self, datapack: &mut Datapack, ctx: &mut CompileContext) {
        match self {
            Self::Expression(expression) => expression.kind.compile_as_statement(datapack, ctx),
            Self::Let(data_type, pattern, value) => {
                let value = value.kind.resolve(datapack, ctx);

                pattern.destructure(datapack, ctx, data_type, value);
            }
            Self::Match(_, _) => {
                todo!()
            }
            Self::Append(target, value) => {
                let target = target.kind.resolve(datapack, ctx);
                let value = value.kind.resolve(datapack, ctx);

                let (target, path) = target.as_data(datapack, ctx, false);

                let modification = value.as_data_command_modification(datapack, ctx);

                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Modify(
                        target.target,
                        path,
                        DataCommandModificationMode::Append,
                        modification,
                    )),
                );
            }
            Self::Remove(expression) => {
                let expression = expression.kind.resolve(datapack, ctx);

                let (target, path) = expression.as_data(datapack, ctx, false);

                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Remove(target.target, path)),
                );
            }
            Self::Break => {
                ctx.add_command(datapack, Command::Return(ReturnCommand::Fail));
            }
            Self::Continue => {
                let LoopInfo {
                    resource_location: loop_resource_location,
                    type_: loop_type,
                } = ctx.loop_info.as_ref().unwrap().clone();

                let iteration_command = Command::Function(loop_resource_location, None);

                let command = match loop_type {
                    LoopType::While(invert, condition) => Command::Execute(ExecuteSubcommand::If(
                        invert,
                        condition.then(ExecuteSubcommand::Run(Box::new(iteration_command))),
                    )),
                    LoopType::Loop => iteration_command,
                };

                ctx.add_command(
                    datapack,
                    Command::Return(ReturnCommand::Run(Box::new(command))),
                );
            }
            Self::Item(item) => item.compile(datapack, ctx),
        }
    }

    pub fn compile(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> Option<LowExpression> {
        match self {
            Self::Expression(expression) => Some(expression.kind.resolve(datapack, ctx)),
            Self::Let(data_type, pattern, value) => {
                let value = value.kind.resolve(datapack, ctx);

                pattern.destructure(datapack, ctx, data_type, value);

                None
            }
            Self::Match(_, _) => {
                todo!()
            }
            Self::Append(target, value) => {
                let target = target.kind.resolve(datapack, ctx);
                let value = value.kind.resolve(datapack, ctx);

                let (target, path) = target.as_data(datapack, ctx, false);

                let modification = value.as_data_command_modification(datapack, ctx);

                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Modify(
                        target.target,
                        path,
                        DataCommandModificationMode::Append,
                        modification,
                    )),
                );

                None
            }
            Self::Remove(expression) => {
                let expression = expression.kind.resolve(datapack, ctx);

                let (target, path) = expression.as_data(datapack, ctx, false);

                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Remove(target.target, path)),
                );

                None
            }
            Self::Break => {
                ctx.add_command(datapack, Command::Return(ReturnCommand::Fail));

                None
            }
            Self::Continue => {
                let LoopInfo {
                    resource_location: loop_resource_location,
                    type_: loop_type,
                } = ctx.loop_info.as_ref().unwrap().clone();

                let iteration_command = Command::Function(loop_resource_location, None);

                let command = match loop_type {
                    LoopType::While(invert, condition) => Command::Execute(ExecuteSubcommand::If(
                        invert,
                        condition.then(ExecuteSubcommand::Run(Box::new(iteration_command))),
                    )),
                    LoopType::Loop => iteration_command,
                };

                ctx.add_command(
                    datapack,
                    Command::Return(ReturnCommand::Run(Box::new(command))),
                );

                None
            }
            Self::Item(item) => {
                item.compile(datapack, ctx);

                None
            }
        }
    }
}
