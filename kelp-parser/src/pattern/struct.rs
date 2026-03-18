use kelp_core::{
    high::snbt_string::SNBTString,
    pattern::{Pattern, PatternKind},
};
use minecraft_command_types::snbt::SNBTString as LowSNBTString;

use crate::{
    cst::{CSTStructPattern, CSTStructPatternField},
    pattern::lower_pattern,
    span::{span_of_cst_node, text_range_to_span},
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_struct_pattern_field(node: CSTStructPatternField) -> Option<(SNBTString, Pattern)> {
    let field_name_token = node.name()?;
    let field_name_span = text_range_to_span(field_name_token.text_range());
    let field_name = field_name_token.text();

    let field_pattern = node.pattern().and_then(lower_pattern).unwrap_or(Pattern {
        span: field_name_span,
        kind: PatternKind::Binding(field_name.to_owned()),
    });

    Some((
        SNBTString {
            snbt_string: LowSNBTString(false, field_name.to_owned()),
            span: field_name_span,
        },
        field_pattern,
    ))
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_struct_pattern(node: CSTStructPattern) -> Option<Pattern> {
    let span = span_of_cst_node(&node);

    let name_token = node.name()?;
    let name = name_token.text();

    let fields = node
        .fields()
        .filter_map(lower_struct_pattern_field)
        .collect();

    Some(PatternKind::Struct(name.to_owned(), fields).with_span(span))
}
