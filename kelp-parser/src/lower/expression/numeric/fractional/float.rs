use ordered_float::NotNan;

use crate::{cst_node, syntax::SyntaxKind};

cst_node!(CSTFloatExpression, SyntaxKind::FloatExpression);

impl<'a> CSTFloatExpression<'a> {
    pub fn value(&self) -> Option<NotNan<f32>> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::FractionalValue {
                token.text.parse().ok()
            } else {
                None
            }
        })
    }
}
