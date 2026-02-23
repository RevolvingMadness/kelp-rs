use ordered_float::NotNan;

use crate::{cst_node, syntax::SyntaxKind};

cst_node!(CSTDoubleExpression, SyntaxKind::DoubleExpression);

impl<'a> CSTDoubleExpression<'a> {
    pub fn value(&self) -> Option<NotNan<f64>> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::FractionalValue {
                token.text.parse().ok()
            } else {
                None
            }
        })
    }
}
