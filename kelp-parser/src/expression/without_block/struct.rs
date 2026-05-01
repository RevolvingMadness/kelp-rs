use std::collections::HashMap;

use minecraft_command_types::snbt::SNBTString as LowSNBTString;

use kelp_core::high::{
    expression::{Expression, ExpressionKind},
    semantic_analysis::SemanticAnalysisContext,
    snbt_string::SNBTString,
};

use crate::{
    cst::{CSTStructExpression, CSTStructExpressionField, CSTStructExpressionFields},
    expression::{lower_expression, try_parse_expression},
    parser::Parser,
    path::generic::lower_generic_path,
    span::{span_of_cst_node, text_range_to_span},
    syntax::SyntaxKind,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
fn lower_struct_expression_field(
    node: CSTStructExpressionField,
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
fn try_parse_struct_expression_field(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !parser.try_bump_identifier_kind(SyntaxKind::StructFieldName)
        && !parser.try_bump_whole_value()
    {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::StructExpressionField);

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
pub fn lower_struct_expression_fields(
    node: CSTStructExpressionFields,
    ctx: &mut SemanticAnalysisContext,
) -> Option<HashMap<SNBTString, Expression>> {
    let fields = node
        .struct_expression_fields()
        .filter_map(|field| lower_struct_expression_field(field, ctx))
        .collect::<_>();

    Some(fields)
}

#[must_use]
pub fn try_parse_struct_expression_fields(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();

    if !try_parse_struct_expression_field(parser) {
        return false;
    }

    parser.start_node_at(checkpoint, SyntaxKind::StructExpressionFields);

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        if !parser.try_bump_char(',') {
            parser.restore_state(state);
            break;
        }

        parser.skip_whitespace();

        if !try_parse_struct_expression_field(parser) {
            break;
        }
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_struct_expression(
    node: CSTStructExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let path = lower_generic_path(node.generic_path()?)?;

    let fields = node
        .struct_expression_fields()
        .and_then(|fields| lower_struct_expression_fields(fields, ctx))
        .unwrap_or_default();

    Some(ExpressionKind::StructStruct(path, fields).with_span(span))
}
