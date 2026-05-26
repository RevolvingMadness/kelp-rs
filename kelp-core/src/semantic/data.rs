use minecraft_command_types::{command::data::DataTarget, resource_location::ResourceLocation};

use crate::{
    compile_context::CompileContext,
    data::{GeneratedData, GeneratedDataTarget},
    datapack::Datapack,
    semantic::{
        coordinate::SemanticCoordinates,
        entity_selector::SemanticEntitySelector,
        nbt_path::{SemanticNbtPath, SemanticNbtPathNode},
        supports_expression_sigil::SemanticSupportsExpressionSigil,
    },
    span::Span,
};

#[derive(Debug, Clone)]
pub struct SemanticData {
    pub target: SemanticDataTarget,
    pub path: SemanticNbtPath,
}

impl SemanticData {
    #[must_use]
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> GeneratedData {
        let target = self.target.compile(datapack, ctx);
        let path = self.path.compile(datapack, ctx);

        GeneratedData { target, path }
    }

    #[inline]
    #[must_use]
    pub fn with_path_node(self, node: SemanticNbtPathNode) -> Self {
        Self {
            path: self.path.with_node(node),
            ..self
        }
    }
}

#[derive(Debug, Clone)]
pub enum SemanticDataTargetKind {
    Block(SemanticSupportsExpressionSigil<Box<SemanticCoordinates>>),
    Entity(SemanticSupportsExpressionSigil<SemanticEntitySelector>),
    Storage(SemanticSupportsExpressionSigil<ResourceLocation>),
}

#[derive(Debug, Clone)]
pub struct SemanticDataTarget {
    pub is_generated: bool,
    pub span: Span,
    pub kind: SemanticDataTargetKind,
}

impl SemanticDataTarget {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> GeneratedDataTarget {
        GeneratedDataTarget {
            is_generated: self.is_generated,
            target: match self.kind {
                SemanticDataTargetKind::Block(coordinates) => {
                    DataTarget::Block(coordinates.compile(datapack, ctx))
                }
                SemanticDataTargetKind::Entity(entity_selector) => {
                    DataTarget::Entity(entity_selector.compile(datapack, ctx))
                }
                SemanticDataTargetKind::Storage(resource_location) => {
                    let resource_location = resource_location.compile(datapack, ctx);

                    DataTarget::Storage(resource_location)
                }
            },
        }
    }
}
