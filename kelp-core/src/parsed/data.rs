use std::fmt::Display;

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    parsed::arena::ParsedAstArena,
    parsed::{
        coordinate::Coordinates, entity_selector::EntitySelector, nbt_path::ParsedNbtPath,
        semantic_analysis::SemanticAnalysisContext,
        supports_expression_sigil::ParsedSupportsExpressionSigil,
    },
    span::Span,
    typed::arena::TypedAstArena,
    typed::data::{TypedData, TypedDataTarget, TypedDataTargetKind},
};

#[derive(Debug, Clone)]
pub struct Data {
    pub target: DataTarget,
    pub path: ParsedNbtPath,
}

impl Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ...", self.target)
    }
}

impl Data {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedData> {
        let target = self
            .target
            .perform_semantic_analysis(parsed_arena, typed_arena, ctx);
        let path = self
            .path
            .perform_semantic_analysis(parsed_arena, typed_arena, ctx);

        let target = target?;
        let path = path?;

        Some(TypedData { target, path })
    }
}

#[derive(Debug, Clone)]
pub enum DataTargetKind {
    Block(Box<ParsedSupportsExpressionSigil<Coordinates>>),
    Entity(ParsedSupportsExpressionSigil<EntitySelector>),
    Storage(ParsedSupportsExpressionSigil<ResourceLocation>),
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

impl Display for DataTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            DataTargetKind::Block(coordinates) => write!(f, "block {}", coordinates),
            DataTargetKind::Entity(selector) => write!(f, "entity {}", selector),
            DataTargetKind::Storage(resource_location) => {
                write!(f, "storage {}", resource_location)
            }
        }
    }
}

impl DataTarget {
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedDataTarget> {
        Some(TypedDataTarget {
            is_generated: self.is_generated,
            kind: match self.kind {
                DataTargetKind::Block(coordinates) => {
                    let coordinates =
                        coordinates.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                    TypedDataTargetKind::Block(coordinates.map(Box::new))
                }
                DataTargetKind::Entity(selector) => {
                    let selector =
                        selector.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                    TypedDataTargetKind::Entity(selector)
                }
                DataTargetKind::Storage(resource_location) => {
                    let resource_location = resource_location.perform_semantic_analysis(
                        parsed_arena,
                        typed_arena,
                        ctx,
                    )?;

                    TypedDataTargetKind::Storage(resource_location)
                }
            },
            span: self.span,
        })
    }
}
