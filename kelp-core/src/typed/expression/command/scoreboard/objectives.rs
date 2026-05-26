use minecraft_command_types::command::{
    enums::scoreboard_render_type::ScoreboardRenderType,
    scoreboard::{ObjectivesScoreboardCommand, ScoreboardModification},
};

use crate::{
    ast_allocator::low::LowAstAllocator,
    compile_context::CompileContext,
    datapack::Datapack,
    typed::expression::{
        TypedExpression, TypedExpressionId,
        command::scoreboard::players::TypedScoreboardNumberFormat,
    },
};

#[derive(Debug, Clone)]
pub enum TypedScoreboardModification {
    DisplayAutoUpdate(bool),
    DisplayName(TypedExpressionId),
    NumberFormat(Option<TypedScoreboardNumberFormat>),
    RenderType(ScoreboardRenderType),
}

impl TypedScoreboardModification {
    #[must_use]
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ScoreboardModification {
        match self {
            Self::DisplayAutoUpdate(auto_update) => {
                ScoreboardModification::DisplayAutoUpdate(auto_update)
            }
            Self::DisplayName(expression) => {
                let snbt = TypedExpression::resolve(expression, allocator, datapack, ctx)
                    .as_snbt_macros(ctx);

                ScoreboardModification::DisplayName(snbt)
            }
            Self::NumberFormat(number_format) => {
                let number_format = number_format
                    .map(|number_format| number_format.compile(allocator, datapack, ctx));

                ScoreboardModification::NumberFormat(number_format)
            }
            Self::RenderType(render_type) => ScoreboardModification::RenderType(render_type),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypedObjectivesScoreboardCommand {
    List,
    Add(String, String, Option<TypedExpressionId>),
    Remove(String),
    SetDisplay(String, Option<String>),
    Modify(String, TypedScoreboardModification),
}

impl TypedObjectivesScoreboardCommand {
    #[must_use]
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ObjectivesScoreboardCommand {
        match self {
            Self::List => ObjectivesScoreboardCommand::List,
            Self::Add(objective, criterion, expression) => {
                let expression = expression.map(|expression| {
                    TypedExpression::resolve(expression, allocator, datapack, ctx)
                        .as_snbt_macros(ctx)
                });

                ObjectivesScoreboardCommand::Add(objective, criterion, expression)
            }
            Self::Remove(objective) => ObjectivesScoreboardCommand::Remove(objective),
            Self::SetDisplay(position, objective) => {
                ObjectivesScoreboardCommand::SetDisplay(position, objective)
            }
            Self::Modify(objective, modification) => {
                let modification = modification.compile(allocator, datapack, ctx);

                ObjectivesScoreboardCommand::Modify(objective, modification)
            }
        }
    }
}
