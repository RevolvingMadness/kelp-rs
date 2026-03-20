use kelp_core::high::{expression::{Expression, ExpressionKind}, semantic_analysis_context::SemanticAnalysisContext, snbt_string::SNBTString};
use minecraft_command_types::snbt::SNBTString as LowSNBTString;

use crate::{
    cst::{CSTStructExpression, CSTStructExpressionField},
    data_type::generics::lower_generic_data_types,
    expression::lower_expression,
    span::{span_of_cst_node, text_range_to_span},
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_struct_expression_field(
    node: CSTStructExpressionField,
    ctx: &mut SemanticAnalysisContext,
) -> Option<(SNBTString, Expression)> {
    let name_token = node.name()?;
    let name_span = name_token.text_range();
    let name = name_token.text();

    let value = lower_expression(node.value()?, ctx)?;

    Some((
        SNBTString {
            span: text_range_to_span(name_span),
            snbt_string: LowSNBTString(false, name.to_owned()),
        },
        value,
    ))
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_struct_expression(
    node: CSTStructExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let name_token = node.name()?;
    let name_span = name_token.text_range();
    let name = name_token.text();

    let generic_data_types = node.generic_data_types().and_then(lower_generic_data_types);

    let fields = node
        .fields()
        .filter_map(|struct_expression_field| {
            lower_struct_expression_field(struct_expression_field, ctx)
        })
        .collect();

    Some(
        ExpressionKind::Struct(
            text_range_to_span(name_span),
            name.to_owned(),
            generic_data_types.unwrap_or_default(),
            fields,
        )
        .with_span(span),
    )
}
