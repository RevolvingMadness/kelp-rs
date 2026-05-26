use minecraft_command_types::{command::data::DataTarget, resource_location::ResourceLocation};

use crate::{
    ast_allocator::low::LowAstAllocator,
    compile_context::CompileContext,
    data::{GeneratedData, GeneratedDataTarget},
    datapack::Datapack,
    span::Span,
    typed::{
        coordinate::TypedCoordinates,
        entity_selector::TypedEntitySelector,
        nbt_path::{TypedNbtPath, TypedNbtPathNode},
        supports_expression_sigil::TypedSupportsExpressionSigil,
    },
};

#[derive(Debug, Clone)]
pub struct TypedData {
    pub target: TypedDataTarget,
    pub path: TypedNbtPath,
}

impl TypedData {
    #[must_use]
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> GeneratedData {
        let target = self.target.compile(allocator, datapack, ctx);
        let path = self.path.compile(allocator, datapack, ctx);

        GeneratedData { target, path }
    }

    #[inline]
    #[must_use]
    pub fn with_path_node(self, node: TypedNbtPathNode) -> Self {
        Self {
            path: self.path.with_node(node),
            ..self
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypedDataTargetKind {
    Block(TypedSupportsExpressionSigil<Box<TypedCoordinates>>),
    Entity(TypedSupportsExpressionSigil<TypedEntitySelector>),
    Storage(TypedSupportsExpressionSigil<ResourceLocation>),
}

impl TypedDataTargetKind {
    #[must_use]
    pub const fn with_regular_span(self, span: Span) -> TypedDataTarget {
        TypedDataTarget {
            is_generated: false,
            span,
            kind: self,
        }
    }

    #[must_use]
    pub const fn with_generated_span(self) -> TypedDataTarget {
        TypedDataTarget {
            is_generated: true,
            span: Span::dummy(),
            kind: self,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedDataTarget {
    pub is_generated: bool,
    pub span: Span,
    pub kind: TypedDataTargetKind,
}

impl TypedDataTarget {
    pub fn compile(
        self,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> GeneratedDataTarget {
        let target = match self.kind {
            TypedDataTargetKind::Block(coordinates) => {
                DataTarget::Block(coordinates.compile(allocator, datapack, ctx))
            }
            TypedDataTargetKind::Entity(entity_selector) => {
                DataTarget::Entity(entity_selector.compile(allocator, datapack, ctx))
            }
            TypedDataTargetKind::Storage(resource_location) => {
                let resource_location = resource_location.compile(allocator, datapack, ctx);

                DataTarget::Storage(resource_location)
            }
        };

        GeneratedDataTarget {
            is_generated: self.is_generated,
            target,
        }
    }
}
