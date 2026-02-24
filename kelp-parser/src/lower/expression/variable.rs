use kelp_core::span::Span;

use crate::{cst_node, syntax::SyntaxKind};

cst_node!(CSTVariableExpression, SyntaxKind::VariableExpression);

impl<'a> CSTVariableExpression<'a> {
    pub fn name_span(&self) -> Option<Span> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some(token.span)
            } else {
                None
            }
        })
    }

    pub fn name<'b>(&self, text: &'b str) -> Option<&'b str> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some(token.text(text))
            } else {
                None
            }
        })
    }
}
