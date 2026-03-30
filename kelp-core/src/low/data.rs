use minecraft_command_types::{
    command::data::DataTarget as LowDataTarget, resource_location::ResourceLocation,
};

use crate::{
    compile_context::CompileContext,
    data::GeneratedDataTarget,
    datapack::Datapack,
    low::{
        coordinate::Coordinates, entity_selector::EntitySelector,
        supports_expression_sigil::SupportsExpressionSigil,
    },
    span::Span,
};

#[derive(Debug, Clone)]
pub enum DataTargetKind {
    Block(Box<Coordinates>),
    Entity(SupportsExpressionSigil<EntitySelector>),
    Storage(SupportsExpressionSigil<ResourceLocation>),
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
