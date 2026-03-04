use kelp_core::high::data::{HighDataTarget, HighDataTargetKind};

use crate::{
    cst::CSTDataTarget,
    lower::{
        coordinates::{lower_coordinates, try_parse_coordinates},
        entity_selector::{lower_entity_selector, try_parse_entity_selector},
        resource_location::{lower_resource_location, try_parse_resource_location},
    },
    parser::Parser,
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
pub fn lower_data_target(node: CSTDataTarget) -> Option<HighDataTarget> {
    let span = span_of_cst_node(&node);

    Some(
        (match node {
            CSTDataTarget::EntityDataTarget(node) => {
                let selector = lower_entity_selector(node.entity_selector()?)?;

                HighDataTargetKind::Entity(selector)
            }
            CSTDataTarget::BlockDataTarget(node) => {
                let coordinates = lower_coordinates(node.coordinates()?)?;

                HighDataTargetKind::Block(coordinates)
            }
            CSTDataTarget::StorageDataTarget(node) => {
                let resource_location = lower_resource_location(node.resource_location()?)?;

                HighDataTargetKind::Storage(resource_location)
            }
        })
        .with_regular_span(span),
    )
}
