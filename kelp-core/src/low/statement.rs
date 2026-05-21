use crate::compile_context::{LoopInfo, LoopType};
use crate::low::data_type::unresolved::UnresolvedDataType;
use crate::low::expression::unresolved::UnresolvedExpression;
use crate::low::item::Item;
use crate::low::pattern::UnresolvedPattern;
use crate::{compile_context::CompileContext, datapack::Datapack};
use minecraft_command_types::command::Command;
use minecraft_command_types::command::data::DataCommandModificationMode;
use minecraft_command_types::command::execute::ExecuteSubcommand;
use minecraft_command_types::command::r#return::ReturnCommand;

#[derive(Debug, Clone)]
pub enum UnresolvedStatement {
    Expression(UnresolvedExpression),
    Let(
        UnresolvedDataType,
        UnresolvedPattern,
        Box<UnresolvedExpression>,
    ),
    Append(UnresolvedExpression, Box<UnresolvedExpression>),
    Remove(UnresolvedExpression),
    Item(Box<Item>),
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub struct ControlFlow {
    pub kind: LoopControlFlowKind,
    pub loop_info: LoopInfo,
}

#[derive(Debug, Clone, Copy)]
pub enum LoopControlFlowKind {
    Break,
    Continue,
}

impl LoopControlFlowKind {
    #[must_use]
    pub const fn name(&self) -> &str {
        match self {
            Self::Break => "break",
            Self::Continue => "continue",
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum EarlyReturnType {
    Break,
    Return,
}

impl UnresolvedStatement {
    #[must_use]
    pub fn get_early_return_type(&self) -> Option<EarlyReturnType> {
        match self {
            Self::Expression(expression) => expression.kind.get_early_return_type(),

            Self::Break => Some(EarlyReturnType::Break),

            _ => None,
        }
    }

    #[must_use]
    pub fn definitely_diverges(&self) -> bool {
        match self {
            Self::Expression(expression) => expression.kind.definitely_diverges(),
            Self::Break | Self::Continue => true,
            Self::Let(_, _, value) => value.kind.definitely_diverges(),
            Self::Append(_, value) => value.kind.definitely_diverges(),
            Self::Remove(expr) => expr.kind.definitely_diverges(),
            Self::Item(..) => false,
        }
    }

    pub fn compile_as_statement(self, datapack: &mut Datapack, ctx: &mut CompileContext) {
        match self {
            Self::Expression(expression) => expression.kind.compile_as_statement(datapack, ctx),
            Self::Let(data_type, pattern, value) => {
                let data_type = data_type.resolve(datapack).unwrap();
                let value = value.kind.resolve(datapack, ctx);

                pattern.destructure(datapack, ctx, data_type, value);
            }
            Self::Append(target, value) => {
                let target = target.kind.resolve(datapack, ctx);
                let value = value.kind.resolve(datapack, ctx);

                let data = target.as_data(datapack, ctx, false);

                let modification = value.as_data_command_modification(datapack, ctx);

                data.modify(
                    datapack,
                    ctx,
                    DataCommandModificationMode::Append,
                    modification,
                );
            }
            Self::Remove(expression) => {
                let expression = expression.kind.resolve(datapack, ctx);

                let data = expression.as_data(datapack, ctx, false);

                data.remove(datapack, ctx);
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
}
