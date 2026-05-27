use std::collections::HashMap;

use kelp_core::{
    parsed::expression::{ParsedExpression, ParsedExpressionKind},
    span::Span,
};

use crate::{
    cst::{CSTStructExpression, CSTStructExpressionField, CSTStructExpressionFields},
    expression::{lower_expression, try_parse_expression},
    extension_traits::{AstNodeExt as _, SyntaxTokenExt},
    lower_context::LowerContext,
    parser::Parser,
    path::generic::lower_generic_path,
    syntax::SyntaxKind,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
fn lower_struct_expression_field(
    node: CSTStructExpressionField,
    ctx: &mut LowerContext,
) -> Option<((Span, String), ParsedExpression)> {
    let name_token = node.name()?;
    let name_span = name_token.span();
    let name = name_token.text();

    let expression = lower_expression(node.expression()?, ctx)?;

    Some(((name_span, name.to_owned()), expression))
}

#[must_use]
fn try_parse_struct_expression_field(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();

    if !parser.try_bump_identifier_kind(SyntaxKind::StructFieldName)
        && !parser.try_bump_whole_value()
    {
        return false;
    }

    checkpoint.start_node(parser, SyntaxKind::StructExpressionField);

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
    ctx: &mut LowerContext,
) -> Option<HashMap<(Span, String), ParsedExpression>> {
    let fields = node
        .struct_expression_fields()
        .filter_map(|field| lower_struct_expression_field(field, ctx))
        .collect::<_>();

    Some(fields)
}

#[must_use]
pub fn try_parse_struct_expression_fields(parser: &mut Parser) -> bool {
    let checkpoint = parser.mark();

    if !try_parse_struct_expression_field(parser) {
        return false;
    }

    checkpoint.start_node(parser, SyntaxKind::StructExpressionFields);

    loop {
        let state = parser.save_state();
        parser.skip_whitespace();

        if !parser.try_bump_char(',') {
            state.restore(parser);
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
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = node.span();

    let path = lower_generic_path(node.generic_path()?)?;

    let fields = node
        .struct_expression_fields()
        .and_then(|fields| lower_struct_expression_fields(fields, ctx))
        .unwrap_or_default();

    Some(ParsedExpressionKind::RegularStruct(path, fields).with_span(span))
}
