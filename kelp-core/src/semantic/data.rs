use minecraft_command_types::{
    command::data::DataTarget as LowDataTarget, resource_location::ResourceLocation,
};

use crate::{
    compile_context::CompileContext,
    data::{GeneratedData, GeneratedDataTarget},
    datapack::Datapack,
    semantic::{
        coordinate::SemanticCoordinates,
        entity_selector::SemanticEntitySelector,
        nbt_path::{NbtPath, NbtPathNode},
        supports_expression_sigil::SemanticSupportsExpressionSigil,
    },
    span::Span,
};

#[derive(Debug, Clone)]
pub struct Data {
    pub target: DataTarget,
    pub path: NbtPath,
}

impl Data {
    #[must_use]
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> GeneratedData {
        let target = self.target.compile(datapack, ctx);
        let path = self.path.compile(datapack, ctx);

        GeneratedData { target, path }
    }

    #[inline]
    #[must_use]
    pub fn with_path_node(self, node: NbtPathNode) -> Self {
        Self {
            path: self.path.with_node(node),
            ..self
        }
    }
}

#[derive(Debug, Clone)]
pub enum DataTargetKind {
    Block(SemanticSupportsExpressionSigil<Box<SemanticCoordinates>>),
    Entity(SemanticSupportsExpressionSigil<SemanticEntitySelector>),
    Storage(SemanticSupportsExpressionSigil<ResourceLocation>),
}

impl DataTargetKind {
    #[must_use]
    pub const fn with_regular_span(self, span: Span) -> DataTarget {
        DataTarget {
            is_generated: false,
            span,
            kind: self,
        }
    }

    #[must_use]
    pub const fn with_generated_span(self) -> DataTarget {
        DataTarget {
            is_generated: true,
            span: Span::dummy(),
            kind: self,
        }
    }
}

#[derive(Debug, Clone)]
pub struct DataTarget {
    pub is_generated: bool,
    pub span: Span,
    pub kind: DataTargetKind,
}

impl DataTarget {
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> GeneratedDataTarget {
        GeneratedDataTarget {
            is_generated: self.is_generated,
            target: match self.kind {
                DataTargetKind::Block(coordinates) => {
                    LowDataTarget::Block(coordinates.compile(datapack, ctx))
                }
                DataTargetKind::Entity(entity_selector) => {
                    LowDataTarget::Entity(entity_selector.compile(datapack, ctx))
                }
                DataTargetKind::Storage(resource_location) => {
                    let resource_location = resource_location.compile(datapack, ctx);

                    LowDataTarget::Storage(resource_location)
                }
            },
        }
    }
}
