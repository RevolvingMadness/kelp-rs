use crate::ast_allocator::low::LowAstAllocator;
use crate::compile_context::{LoopInfo, LoopType};
use crate::low::data_type::unresolved::UnresolvedDataType;
use crate::low::expression::unresolved::{UnresolvedExpression, UnresolvedExpressionId};
use crate::low::item::Item;
use crate::low::pattern::UnresolvedPattern;
use crate::{compile_context::CompileContext, datapack::Datapack};
use la_arena::Idx;
use minecraft_command_types::command::Command;
use minecraft_command_types::command::execute::ExecuteSubcommand;
use minecraft_command_types::command::r#return::ReturnCommand;

#[derive(Debug, Clone)]
pub enum UnresolvedStatement {
    Expression(UnresolvedExpressionId),
    Let(
        UnresolvedDataType,
        Idx<UnresolvedPattern>,
        UnresolvedExpressionId,
    ),
    Append(UnresolvedExpressionId, UnresolvedExpressionId),
    Remove(UnresolvedExpressionId),
    Item(Idx<Item>),
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
    pub fn get_early_return_type(
        id: Idx<Self>,
        allocator: &LowAstAllocator,
    ) -> Option<EarlyReturnType> {
        match allocator.get_statement(id) {
            Self::Expression(expression) => {
                UnresolvedExpression::get_early_return_type(*expression, allocator)
            }

            Self::Break => Some(EarlyReturnType::Break),

            _ => None,
        }
    }

    #[must_use]
    pub fn definitely_diverges(id: Idx<Self>, allocator: &LowAstAllocator) -> bool {
        match allocator.get_statement(id) {
            Self::Expression(expression) => {
                UnresolvedExpression::definitely_diverges(*expression, allocator)
            }
            Self::Break | Self::Continue => true,
            Self::Let(_, _, value) => UnresolvedExpression::definitely_diverges(*value, allocator),
            Self::Append(_, value) => UnresolvedExpression::definitely_diverges(*value, allocator),
            Self::Remove(expr) => UnresolvedExpression::definitely_diverges(*expr, allocator),
            Self::Item(..) => false,
        }
    }

    pub fn compile_as_statement(
        id: Idx<Self>,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) {
        match allocator.get_statement(id) {
            Self::Expression(expression) => {
                UnresolvedExpression::compile_as_statement(*expression, allocator, datapack, ctx);
            }
            Self::Let(data_type, pattern, value) => {
                let data_type = data_type.clone().resolve(datapack).unwrap();
                let value = UnresolvedExpression::resolve(*value, allocator, datapack, ctx);

                UnresolvedPattern::destructure(
                    *pattern, allocator, datapack, ctx, data_type, value,
                );
            }
            Self::Append(target, value) => {
                let target = UnresolvedExpression::resolve(*target, allocator, datapack, ctx);
                let value = UnresolvedExpression::resolve(*value, allocator, datapack, ctx);

                let data = target.as_data(datapack, ctx, false);

                let modification = value.as_data_command_modification(datapack, ctx);

                ctx.add_command(datapack, data.append(modification));
            }
            Self::Remove(expression) => {
                let expression =
                    UnresolvedExpression::resolve(*expression, allocator, datapack, ctx);

                let data = expression.as_data(datapack, ctx, false);

                ctx.add_command(datapack, data.remove());
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
            Self::Item(item) => Item::compile(*item, allocator, datapack, ctx),
        }
    }
}
