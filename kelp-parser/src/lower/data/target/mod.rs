use kelp_core::{
    high::data::{HighDataTarget, HighDataTargetKind},
    span::Span,
};

use crate::{
    cstlib::CSTNodeType,
    lower::{
        coordinates::CSTCoordinates,
        data::target::{
            block::CSTBlockDataTarget, entity::CSTEntityDataTarget, storage::CSTStorageDataTarget,
        },
        entity_selector::CSTEntitySelector,
        resource_location::CSTResourceLocation,
    },
    parser::Parser,
    semantic_token::SemanticToken,
    syntax::SyntaxKind,
};

pub mod block;
pub mod entity;
pub mod storage;

#[derive(Debug)]
pub enum CSTDataTargetKind<'a> {
    Entity(CSTEntityDataTarget<'a>),
    Block(CSTBlockDataTarget<'a>),
    Storage(CSTStorageDataTarget<'a>),
}

impl<'a> CSTDataTargetKind<'a> {
    #[inline]
    #[must_use]
    pub fn with_span(self, span: Span) -> CSTDataTarget<'a> {
        CSTDataTarget { span, kind: self }
    }
}

#[derive(Debug)]
pub struct CSTDataTarget<'a> {
    pub span: Span,
    pub kind: CSTDataTargetKind<'a>,
}

impl<'a> CSTDataTarget<'a> {
    pub fn try_parse(parser: &mut Parser) -> bool {
        let Some(identifier) = parser.peek_identifier() else {
            return false;
        };

        let state = parser.save_state();

        match identifier {
            "entity" => {
                parser.start_node(SyntaxKind::EntityDataTarget);

                parser.bump_identifier("entity");

                if !parser.expect_inline_whitespace() || !CSTEntitySelector::try_parse(parser) {
                    parser.restore_state(state);

                    return false;
                }
            }
            "block" => {
                parser.start_node(SyntaxKind::BlockDataTarget);

                parser.bump_identifier("block");

                if !parser.expect_inline_whitespace() || !CSTCoordinates::try_parse(parser) {
                    parser.restore_state(state);

                    return false;
                }
            }
            "storage" => {
                parser.start_node(SyntaxKind::StorageDataTarget);

                parser.bump_identifier("storage");

                if !parser.expect_inline_whitespace() || !CSTResourceLocation::try_parse(parser) {
                    parser.restore_state(state);

                    return false;
                }
            }
            _ => return false,
        }

        parser.finish_node();

        true
    }

    #[must_use]
    pub fn cast(node: &'a CSTNodeType) -> Option<CSTDataTarget<'a>> {
        Some(
            (match node.kind()? {
                SyntaxKind::EntityDataTarget => {
                    CSTDataTargetKind::Entity(CSTEntityDataTarget::cast(node)?)
                }
                SyntaxKind::BlockDataTarget => {
                    CSTDataTargetKind::Block(CSTBlockDataTarget::cast(node)?)
                }
                SyntaxKind::StorageDataTarget => {
                    CSTDataTargetKind::Storage(CSTStorageDataTarget::cast(node)?)
                }
                _ => {
                    #[cfg(debug_assertions)]
                    println!("Failed to cast node {:?} to CSTDataTarget", node);

                    return None;
                }
            })
            .with_span(node.span()),
        )
    }

    #[must_use]
    pub fn lower(self, text: &str) -> Option<HighDataTarget> {
        Some(
            (match self.kind {
                CSTDataTargetKind::Entity(target) => {
                    let selector = target.selector()?.lower(text)?;

                    HighDataTargetKind::Entity(selector)
                }
                CSTDataTargetKind::Block(target) => {
                    let coordinates = target.coordinates()?.lower(text)?;

                    HighDataTargetKind::Block(coordinates)
                }
                CSTDataTargetKind::Storage(target) => {
                    let resource_location = target.resource_location()?.lower(text)?;

                    HighDataTargetKind::Storage(resource_location)
                }
            })
            .with_regular_span(self.span),
        )
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        match &self.kind {
            CSTDataTargetKind::Entity(target) => {
                target.collect_semantic_tokens(tokens);
            }
            CSTDataTargetKind::Block(target) => {
                target.collect_semantic_tokens(tokens);
            }
            CSTDataTargetKind::Storage(target) => {
                target.collect_semantic_tokens(tokens);
            }
        }
    }
}
