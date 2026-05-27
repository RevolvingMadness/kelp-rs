use kelp_core::parsed::pattern::{ParsedPattern, ParsedPatternKind};

use crate::{cst::CSTWildcardPattern, extension_traits::AstNodeExt as _};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_wildcard_pattern(node: CSTWildcardPattern) -> Option<ParsedPattern> {
    Some(ParsedPatternKind::Wildcard.with_span(node.span()))
}
