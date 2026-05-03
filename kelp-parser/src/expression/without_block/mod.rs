use kelp_core::high::{expression::Expression, semantic_analysis::SemanticAnalysisContext};

use crate::{
    cst::CSTExpressionWithoutBlock,
    expression::without_block::{
        as_cast::lower_as_cast_expression, assignment::lower_assignment_expression,
        binary::lower_binary_expression, boolean::lower_boolean_expression,
        call::lower_call_expression, character::lower_character_expression,
        command::lower_command_expression, compound::lower_compound_expression,
        coordinates::lower_coordinates_expression, data::lower_data_expression,
        entity_selector::lower_entity_selector_expression,
        field_access::lower_field_access_expression, index::lower_index_expression,
        list::lower_list_expression, numeric::lower_numeric_expression,
        parenthesized::lower_parenthesized_expression, path::lower_path_expression,
        resource_location::lower_resource_location_expression, r#return::lower_return_expression,
        score::lower_score_expression, string::lower_string_expression,
        r#struct::lower_struct_expression, to_cast::lower_to_cast_expression,
        tuple::lower_tuple_expression, unary::lower_unary_expression,
        underscore::lower_underscore_expression, unit::lower_unit_expression,
    },
};

pub mod as_cast;
pub mod assignment;
pub mod binary;
pub mod boolean;
pub mod call;
pub mod character;
pub mod command;
pub mod compound;
pub mod coordinates;
pub mod data;
pub mod entity_selector;
pub mod field_access;
pub mod index;
pub mod list;
pub mod numeric;
pub mod parenthesized;
pub mod path;
pub mod resource_location;
pub mod r#return;
pub mod score;
pub mod string;
pub mod r#struct;
pub mod to_cast;
pub mod tuple;
pub mod unary;
pub mod underscore;
pub mod unit;

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_expression_without_block(
    node: CSTExpressionWithoutBlock,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    match node {
        CSTExpressionWithoutBlock::UnaryExpression(node) => lower_unary_expression(node, ctx),
        CSTExpressionWithoutBlock::PathExpression(node) => lower_path_expression(node, ctx),
        CSTExpressionWithoutBlock::UnderscoreExpression(node) => {
            lower_underscore_expression(node, ctx)
        }
        CSTExpressionWithoutBlock::BooleanExpression(node) => lower_boolean_expression(node, ctx),
        CSTExpressionWithoutBlock::NumericExpression(node) => lower_numeric_expression(node, ctx),
        CSTExpressionWithoutBlock::CharacterExpression(node) => {
            lower_character_expression(node, ctx)
        }
        CSTExpressionWithoutBlock::StringExpression(node) => lower_string_expression(node, ctx),
        CSTExpressionWithoutBlock::AssignmentExpression(node) => {
            lower_assignment_expression(node, ctx)
        }
        CSTExpressionWithoutBlock::BinaryExpression(node) => lower_binary_expression(node, ctx),
        CSTExpressionWithoutBlock::DataExpression(node) => lower_data_expression(node, ctx),
        CSTExpressionWithoutBlock::ScoreExpression(node) => lower_score_expression(node, ctx),
        CSTExpressionWithoutBlock::CommandExpression(node) => lower_command_expression(node, ctx),
        CSTExpressionWithoutBlock::UnitExpression(node) => lower_unit_expression(node, ctx),
        CSTExpressionWithoutBlock::CompoundExpression(node) => lower_compound_expression(node, ctx),
        CSTExpressionWithoutBlock::ListExpression(node) => lower_list_expression(node, ctx),
        CSTExpressionWithoutBlock::TupleExpression(node) => lower_tuple_expression(node, ctx),
        CSTExpressionWithoutBlock::CallExpression(node) => lower_call_expression(node, ctx),
        CSTExpressionWithoutBlock::ParenthesizedExpression(node) => {
            lower_parenthesized_expression(node, ctx)
        }
        CSTExpressionWithoutBlock::AsCastExpression(node) => lower_as_cast_expression(node, ctx),
        CSTExpressionWithoutBlock::ToCastExpression(node) => lower_to_cast_expression(node, ctx),
        CSTExpressionWithoutBlock::StructExpression(node) => lower_struct_expression(node, ctx),
        CSTExpressionWithoutBlock::IndexExpression(node) => lower_index_expression(node, ctx),
        CSTExpressionWithoutBlock::FieldAccessExpression(node) => {
            lower_field_access_expression(node, ctx)
        }
        CSTExpressionWithoutBlock::ReturnExpression(node) => lower_return_expression(node, ctx),
        CSTExpressionWithoutBlock::ResourceLocationExpression(node) => {
            lower_resource_location_expression(node, ctx)
        }
        CSTExpressionWithoutBlock::EntitySelectorExpression(node) => {
            lower_entity_selector_expression(node, ctx)
        }
        CSTExpressionWithoutBlock::CoordinatesExpression(node) => {
            lower_coordinates_expression(node, ctx)
        }
    }
}
