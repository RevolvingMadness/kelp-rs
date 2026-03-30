use kelp_core::high::{
    data::{DataTarget, DataTargetKind},
    semantic_analysis::SemanticAnalysisContext,
};

use crate::{
    coordinates::{lower_coordinates, try_parse_coordinates},
    cst::CSTDataTarget,
    entity_selector::{lower_entity_selector, try_parse_entity_selector},
    parser::Parser,
    resource_location::{lower_resource_location, try_parse_resource_location},
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

pub fn try_parse_data_target(parser: &mut Parser) -> bool {
    let Some(identifier) = parser.peek_identifier() else {
        return false;
    };

    let state = parser.save_state();

    match identifier {
        "entity" => {
            parser.start_node(SyntaxKind::EntityDataTarget);

            parser.bump_identifier_kind(SyntaxKind::EntityKeyword, "entity");

            if !parser.expect_inline_whitespace() || !try_parse_entity_selector(parser) {
                parser.restore_state(state);

                return false;
            }
        }
        "block" => {
            parser.start_node(SyntaxKind::BlockDataTarget);

            parser.bump_identifier_kind(SyntaxKind::BlockKeyword, "block");

            if !parser.expect_inline_whitespace() || !try_parse_coordinates(parser) {
                parser.restore_state(state);

                return false;
            }
        }
        "storage" => {
            parser.start_node(SyntaxKind::StorageDataTarget);

            parser.bump_identifier_kind(SyntaxKind::StorageKeyword, "storage");

            if !parser.expect_inline_whitespace() || !try_parse_resource_location(parser) {
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
pub fn lower_data_target(
    node: CSTDataTarget,
    ctx: &mut SemanticAnalysisContext,
) -> Option<DataTarget> {
    let span = span_of_cst_node(&node);

    Some(
        (match node {
            CSTDataTarget::EntityDataTarget(node) => {
                let selector = lower_entity_selector(node.entity_selector()?, ctx)?;

                DataTargetKind::Entity(selector)
            }
            CSTDataTarget::BlockDataTarget(node) => {
                let coordinates = lower_coordinates(node.coordinates()?, ctx)?;

                DataTargetKind::Block(Box::new(coordinates))
            }
            CSTDataTarget::StorageDataTarget(node) => {
                let resource_location = lower_resource_location(node.resource_location()?, ctx)?;

                DataTargetKind::Storage(resource_location)
            }
        })
        .with_regular_span(span),
    )
}
