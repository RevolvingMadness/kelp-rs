use std::collections::HashMap;

use minecraft_command_types::snbt::SNBTString as LowSNBTString;

use kelp_core::high::{
    expression::{Expression, ExpressionKind},
    semantic_analysis::SemanticAnalysisContext,
    snbt_string::SNBTString,
};

use crate::{
    cst::{
        CSTStructStructExpression, CSTStructStructExpressionField, CSTStructStructExpressionFields,
        CSTTupleStructExpression, CSTTupleStructExpressionField, CSTTupleStructExpressionFields,
    },
    expression::{lower_expression, try_parse_expression},
    parser::Parser,
    path::generic::lower_generic_path,
    span::{span_of_cst_node, text_range_to_span},
    syntax::SyntaxKind,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
fn lower_struct_struct_expression_field(
    node: CSTStructStructExpressionField,
    ctx: &mut SemanticAnalysisContext,
) -> Option<(SNBTString, Expression)> {
    let name_token = node.name()?;
    let name_span = text_range_to_span(name_token.text_range());
    let name = name_token.text();

    let expression = lower_expression(node.expression()?, ctx)?;

    Some((
        SNBTString {
            snbt_string: LowSNBTString(false, name.to_owned()),
            span: name_span,
        },
        expression,
    ))
}

#[must_use]
fn try_parse_struct_struct_expression_field(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !parser.try_bump_identifier_kind(SyntaxKind::StructFieldName)
        && !parser.try_bump_whole_value()
    {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::StructStructExpressionField);

    parser.skip_whitespace();

    let parsed_colon = parser.expect_char(':', "Expected ':'");

    parser.skip_whitespace();

    if !try_parse_expression(parser) && parsed_colon {
        parser.error("Expected expression");
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_struct_struct_expression_fields(
    node: CSTStructStructExpressionFields,
    ctx: &mut SemanticAnalysisContext,
) -> Option<HashMap<SNBTString, Expression>> {
    let fields = node
        .struct_struct_expression_fields()
        .filter_map(|field| lower_struct_struct_expression_field(field, ctx))
        .collect::<_>();

    Some(fields)
}

#[must_use]
pub fn try_parse_struct_struct_expression_fields(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_struct_struct_expression_field(parser) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::StructStructExpressionFields);

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        if !parser.try_bump_char(',') {
            parser.restore_state(state);
            break;
        }

        parser.skip_whitespace();

        if !try_parse_struct_struct_expression_field(parser) {
            break;
        }
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_struct_struct_expression(
    node: CSTStructStructExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let path = lower_generic_path(node.generic_path()?)?;

    let fields = node
        .struct_struct_expression_fields()
        .and_then(|fields| lower_struct_struct_expression_fields(fields, ctx))
        .unwrap_or_default();

    Some(ExpressionKind::StructStruct(path, fields).with_span(span))
}

#[must_use]
fn try_parse_tuple_struct_expression_field(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_expression(parser) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::TupleStructExpressionField);

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
fn lower_tuple_struct_expression_field(
    node: CSTTupleStructExpressionField,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let expression = lower_expression(node.expression()?, ctx)?;

    Some(expression)
}

#[must_use]
pub fn try_parse_tuple_struct_expression_fields(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_tuple_struct_expression_field(parser) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::TupleStructExpressionFields);

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        if !parser.try_bump_char(',') {
            parser.restore_state(state);
            break;
        }

        parser.skip_whitespace();
        if !try_parse_tuple_struct_expression_field(parser) {
            break;
        }
    }

    parser.finish_node();
    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_tuple_struct_expression_fields(
    node: CSTTupleStructExpressionFields,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Vec<Expression>> {
    let fields = node
        .tuple_struct_expression_fields()
        .filter_map(|field| lower_tuple_struct_expression_field(field, ctx))
        .collect();

    Some(fields)
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_tuple_struct_expression(
    node: CSTTupleStructExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let path = lower_generic_path(node.generic_path()?)?;

    let fields = node
        .tuple_struct_expression_fields()
        .and_then(|fields| lower_tuple_struct_expression_fields(fields, ctx))
        .unwrap_or_default();

    Some(ExpressionKind::TupleStruct(path, fields).with_span(span))
}
