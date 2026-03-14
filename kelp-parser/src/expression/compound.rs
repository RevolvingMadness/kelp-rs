use std::collections::BTreeMap;

use kelp_core::{
    high::expression::{Expression, ExpressionKind},
    high::snbt_string::SNBTString,
    semantic_analysis_context::SemanticAnalysisContext,
    span::Span,
};
use minecraft_command_types::snbt::SNBTString as LowSNBTString;

use crate::{
    cst::CSTCompoundExpression,
    expression::{lower_expression, try_parse_expression},
    parser::Parser,
    span::{span_of_cst_node, text_range_to_span},
    syntax::SyntaxKind,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn try_parse_compound_expression(parser: &mut Parser) -> bool {
    if parser.peek_char() != Some('{') {
        return false;
    }

    parser.start_node(SyntaxKind::CompoundExpression);
    parser.bump_char();

    loop {
        parser.skip_whitespace();

        if parser.is_eof() || parser.peek_char() == Some('}') {
            break;
        }

        parser.start_node(SyntaxKind::CompoundExpressionEntry);

        if !parser.try_parse_string_or_identifier_kind(SyntaxKind::CompoundKey) {
            parser.error("Expected compound key");
            bump_until_next_compound_entry_or_end(parser);
            parser.finish_node();

            continue;
        }

        parser.skip_whitespace();

        if !parser.expect_char(':', "Expected ':'") {
            bump_until_next_compound_entry_or_end(parser);
            parser.finish_node();

            continue;
        }

        parser.skip_whitespace();

        if !try_parse_expression(parser) {
            parser.error("Expected expression");

            bump_until_next_compound_entry_or_end(parser);

            parser.finish_node();

            continue;
        }

        parser.finish_node();
        parser.skip_whitespace();

        if !parser.try_bump_char(',') && parser.peek_char() != Some('}') {
            parser.error("Expected ',' or '}'");
            bump_until_next_compound_entry_or_end(parser);
        }
    }

    parser.expect_char('}', "Expected '}'");
    parser.finish_node();

    true
}

fn bump_until_next_compound_entry_or_end(parser: &mut Parser) {
    let chars = parser.source[parser.pos..].chars();
    let mut length = 0;

    for char in chars {
        if char == ',' || char == '}' || char == '\n' {
            break;
        }

        length += char.len_utf8();
    }

    if length > 0 {
        parser.add_token(SyntaxKind::Garbage, length);
    }

    if parser.peek_char() == Some(',') {
        parser.bump_char();
    }
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_compound_expression_inner(
    node: CSTCompoundExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<(Span, BTreeMap<SNBTString, Expression>)> {
    let mut compound = BTreeMap::new();

    for entry in node.entries() {
        let Some(key_token) = entry.name() else {
            continue;
        };

        let Some(value) = entry.value() else {
            continue;
        };
        let Some(value) = lower_expression(value, ctx) else {
            continue;
        };

        let key_span = text_range_to_span(key_token.text_range());
        let key = key_token.text().to_owned();

        compound.insert(
            SNBTString {
                span: key_span,
                snbt_string: LowSNBTString(false, key),
            },
            value,
        );
    }

    Some((span_of_cst_node(&node), compound))
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_compound_expression(
    node: CSTCompoundExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let (span, compound) = lower_compound_expression_inner(node, ctx)?;

    Some(ExpressionKind::Compound(compound).with_span(span))
}
