use ordered_float::NotNan;

use crate::{cst_node, syntax::SyntaxKind};

cst_node!(CSTDoubleExpression, SyntaxKind::DoubleExpression);

impl CSTDoubleExpression<'_> {
    #[must_use]
    pub fn value(&self, text: &str) -> Option<NotNan<f64>> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::FractionalValue {
                token.text(text).parse().ok()
            } else {
                None
            }
        })
    }
}
