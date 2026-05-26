use kelp_core::parsed::pattern::{ParsedPattern, ParsedPatternKind};

use crate::{cst::CSTBindingPattern, path::generic::lower_generic_path, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_binding_pattern(node: CSTBindingPattern) -> Option<ParsedPattern> {
    let span = span_of_cst_node(&node);

    let path = lower_generic_path(node.generic_path()?)?;

    Some(ParsedPatternKind::Binding(path).with_span(span))
}
