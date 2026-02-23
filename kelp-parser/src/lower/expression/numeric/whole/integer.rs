use crate::{cst_node, syntax::SyntaxKind};

cst_node!(CSTIntegerExpression, SyntaxKind::IntegerExpression);

impl<'a> CSTIntegerExpression<'a> {
    pub fn value(&self) -> Option<i32> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::WholeValue {
                token.text.parse().ok()
            } else {
                None
            }
        })
    }
}
