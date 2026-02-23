use kelp_core::span::Span;

use crate::{cst_node, lower::pattern::CSTPattern, syntax::SyntaxKind};

cst_node!(CSTStructPattern, SyntaxKind::StructPattern);

impl<'a> CSTStructPattern<'a> {
    pub fn name(&self) -> Option<&'a str> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some(token.text)
            } else {
                None
            }
        })
    }

    pub fn fields(&self) -> Vec<CSTStructPatternField<'a>> {
        self.0
            .children()
            .filter_map(CSTStructPatternField::cast)
            .collect()
    }
}

cst_node!(CSTStructPatternField, SyntaxKind::StructPatternField);

impl<'a> CSTStructPatternField<'a> {
    pub fn name(&self) -> Option<(Span, &'a str)> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some((token.span, token.text))
            } else {
                None
            }
        })
    }

    pub fn pattern(&self) -> Option<CSTPattern<'a>> {
        self.0.children().find_map(CSTPattern::cast)
    }
}
