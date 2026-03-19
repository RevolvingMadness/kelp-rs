use kelp_core::high::pattern::{Pattern, PatternKind};

use crate::{cst::CSTBindingPattern, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_binding_pattern(node: CSTBindingPattern) -> Option<Pattern> {
    let span = span_of_cst_node(&node);

    let name_token = node.binding_pattern_name_token()?;
    let name = name_token.text();

    Some(PatternKind::Binding(name.to_string()).with_span(span))
}
