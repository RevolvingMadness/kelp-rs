use kelp_core::span::Span;

use crate::{cst_node, lower::expression::CSTExpression, syntax::SyntaxKind};

cst_node!(CSTStructField, SyntaxKind::StructField);

impl<'a> CSTStructField<'a> {
    pub fn name(&self) -> Option<(Span, &'a str)> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some((token.span, token.text))
            } else {
                None
            }
        })
    }

    pub fn value(&self) -> Option<CSTExpression<'a>> {
        self.0.children().find_map(CSTExpression::cast)
    }
}
