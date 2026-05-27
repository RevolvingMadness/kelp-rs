use kelp_core::parsed::data::{ParsedDataTarget, ParsedDataTargetKind};

use crate::{
    cst::{CSTCoordinates, CSTDataTarget, CSTEntitySelector, CSTResourceLocation},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTDataTarget {
    fn try_parse(parser: &mut Parser) -> bool {
        let Some(identifier) = parser.peek_identifier() else {
            return false;
        };

        let state = parser.save_state();

        match identifier {
            "entity" => {
                parser.start_node(SyntaxKind::EntityDataTarget);

                parser.bump_identifier_kind(SyntaxKind::EntityKeyword, "entity");

                if !parser.expect_inline_whitespace() || !CSTEntitySelector::try_parse(parser) {
                    state.restore(parser);

                    return false;
                }
            }
            "block" => {
                parser.start_node(SyntaxKind::BlockDataTarget);

                parser.bump_identifier_kind(SyntaxKind::BlockKeyword, "block");

                if !parser.expect_inline_whitespace() || !CSTCoordinates::try_parse(parser) {
                    state.restore(parser);

                    return false;
                }
            }
            "storage" => {
                parser.start_node(SyntaxKind::StorageDataTarget);

                parser.bump_identifier_kind(SyntaxKind::StorageKeyword, "storage");

                if !parser.expect_inline_whitespace() || !CSTResourceLocation::try_parse(parser) {
                    state.restore(parser);

                    return false;
                }
            }
            _ => return false,
        }

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTDataTarget {
    type Lowered = ParsedDataTarget;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let span = self.span();

        Some(
            (match self {
                Self::EntityDataTarget(node) => {
                    let selector = node.entity_selector()?.lower(ctx)?;

                    ParsedDataTargetKind::Entity(selector)
                }
                Self::BlockDataTarget(node) => {
                    let coordinates = node.coordinates()?.lower(ctx)?;

                    ParsedDataTargetKind::Block(Box::new(coordinates))
                }
                Self::StorageDataTarget(node) => {
                    let resource_location = node.resource_location()?.lower(ctx)?;

                    ParsedDataTargetKind::Storage(resource_location)
                }
            })
            .with_regular_span(span),
        )
    }
}
