use kelp_core::high::pattern::{Pattern, PatternKind};

use crate::{cst::CSTBindingPattern, path::generic::lower_generic_path, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_binding_pattern(node: CSTBindingPattern) -> Option<Pattern> {
    let span = span_of_cst_node(&node);

    let path = lower_generic_path(node.generic_path()?)?;

    Some(PatternKind::Binding(path).with_span(span))
}
