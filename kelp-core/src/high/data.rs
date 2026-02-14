use minecraft_command_types::{
    command::data::DataTarget, coordinate::Coordinates, resource_location::ResourceLocation,
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext, datapack::HighDatapack,
    high::entity_selector::HighEntitySelector, semantic_analysis_context::SemanticAnalysisContext,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct GeneratedDataTarget {
    pub is_generated: bool,
    pub target: DataTarget,
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighDataTargetKind {
    Block(Coordinates),
    Entity(HighEntitySelector),
    Storage(ResourceLocation),
}

impl HighDataTargetKind {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            HighDataTargetKind::Entity(selector) => selector.perform_semantic_analysis(ctx, is_lhs),
            _ => Some(()),
        }
    }

    #[inline]
    #[must_use]
    pub fn into_generated(self) -> HighDataTarget {
        HighDataTarget {
            is_generated: true,
            kind: self,
        }
    }

    #[inline]
    #[must_use]
    pub fn into_regular(self) -> HighDataTarget {
        HighDataTarget {
            is_generated: false,
            kind: self,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct HighDataTarget {
    pub is_generated: bool,
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
