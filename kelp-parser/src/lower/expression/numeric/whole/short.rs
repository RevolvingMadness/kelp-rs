use crate::{cst_node, syntax::SyntaxKind};

cst_node!(CSTShortExpression, SyntaxKind::ShortExpression);

impl<'a> CSTShortExpression<'a> {
    pub fn value(&self) -> Option<i16> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::WholeValue {
                token.text.parse().ok()
            } else {
                None
            }
        })
    }
}
