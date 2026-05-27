use kelp_core::parsed::pattern::{ParsedPattern, ParsedPatternKind};

use crate::{
    cst::CSTBindingPattern, extension_traits::AstNodeExt, path::generic::lower_generic_path,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_binding_pattern(node: CSTBindingPattern) -> Option<ParsedPattern> {
    let span = node.span();

    let path = lower_generic_path(node.generic_path()?)?;

    Some(ParsedPatternKind::Binding(path).with_span(span))
}
