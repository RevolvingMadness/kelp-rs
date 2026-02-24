use crate::{cst_node, syntax::SyntaxKind};

cst_node!(CSTIntegerExpression, SyntaxKind::IntegerExpression);

impl CSTIntegerExpression<'_> {
    #[must_use]
    pub fn value(&self, text: &str) -> Option<i32> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::WholeValue {
                token.text(text).parse().ok()
            } else {
                None
            }
        })
    }
}
