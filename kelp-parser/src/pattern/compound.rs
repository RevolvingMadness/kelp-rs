use kelp_core::{
    parsed::pattern::{ParsedPattern, ParsedPatternKind},
    parsed::typed_path::ParsedTypedPath,
    span::Span,
};

use crate::{
    cst::{CSTCompoundPattern, CSTCompoundPatternEntry},
    extension_traits::{AstNodeExt, LowerableAstNode, SyntaxTokenExt},
    lower_context::LowerContext,
};

impl LowerableAstNode for CSTCompoundPatternEntry {
    type Lowered = ((Span, String), ParsedPattern);

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let name_token = self.name()?;
        let name_span = name_token.span();
        let name = name_token.text();

        let entry_pattern = self
            .pattern()
            .and_then(|pattern| pattern.lower(ctx))
            .unwrap_or_else(|| ParsedPattern {
                span: name_span,
                kind: ParsedPatternKind::Binding(ParsedTypedPath::single(name_span, name)),
            });

        Some(((name_span, name.to_owned()), entry_pattern))
    }
}

impl LowerableAstNode for CSTCompoundPattern {
    type Lowered = ParsedPattern;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let entries = self
            .entries()
            .filter_map(|entry| entry.lower(ctx))
            .collect();

        Some(ParsedPatternKind::Compound(entries).with_span(self.span()))
    }
}
