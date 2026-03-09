use minecraft_command_types::{
    command::{
        Command,
        data::{DataCommand, DataCommandModification, DataCommandModificationMode, DataTarget},
    },
    coordinate::Coordinates,
    nbt_path::NbtPath,
    resource_location::ResourceLocation,
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext, datapack::HighDatapack,
    high::entity_selector::HighEntitySelector, semantic_analysis_context::SemanticAnalysisContext,
    span::Span,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct GeneratedDataTarget {
    pub is_generated: bool,
    pub target: DataTarget,
}

impl GeneratedDataTarget {
    #[must_use]
    pub fn as_unique_data(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        path: NbtPath,
    ) -> (Self, NbtPath) {
        let (unique_data, unique_path) = datapack.get_unique_data();

        ctx.add_command(
            datapack,
            Command::Data(DataCommand::Modify(
                unique_data.target.clone(),
                unique_path.clone(),
                DataCommandModificationMode::Set,
                DataCommandModification::From(self.target, Some(path)),
            )),
        );

        (unique_data, unique_path)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighDataTargetKind {
    Block(Coordinates),
    Entity(HighEntitySelector),
    Storage(ResourceLocation),
}

impl HighDataTargetKind {
    #[must_use]
    pub const fn with_regular_span(self, span: Span) -> HighDataTarget {
        HighDataTarget {
            is_generated: false,
            span,
            kind: self,
        }
    }

    #[must_use]
    pub const fn with_generated_span(self) -> HighDataTarget {
        HighDataTarget {
            is_generated: true,
            span: Span::dummy(),
            kind: self,
        }
    }

    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            Self::Entity(selector) => selector.perform_semantic_analysis(ctx, is_lhs),
            _ => Some(()),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct HighDataTarget {
    pub is_generated: bool,
    pub span: Span,
    pub kind: HighDataTargetKind,
}

impl HighDataTarget {
    pub fn compile(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> GeneratedDataTarget {
        GeneratedDataTarget {
            is_generated: self.is_generated,
            target: match self.kind {
                HighDataTargetKind::Block(coordinates) => DataTarget::Block(coordinates),
                HighDataTargetKind::Entity(entity_selector) => {
                    DataTarget::Entity(entity_selector.compile(datapack, ctx))
                }
                HighDataTargetKind::Storage(resource_location) => {
                    DataTarget::Storage(resource_location)
                }
            },
        }
    }
}
