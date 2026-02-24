use crate::{cst_node, syntax::SyntaxKind};

cst_node!(CSTLongExpression, SyntaxKind::LongExpression);

impl<'a> CSTLongExpression<'a> {
    pub fn value(&self, text: &str) -> Option<i64> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::WholeValue {
                token.text(text).parse().ok()
            } else {
                None
            }
        })
    }
}
