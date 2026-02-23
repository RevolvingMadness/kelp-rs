use crate::{cst_node, syntax::SyntaxKind};

cst_node!(CSTByteExpression, SyntaxKind::ByteExpression);

impl<'a> CSTByteExpression<'a> {
    pub fn value(&self) -> Option<i8> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::WholeValue {
                token.text.parse().ok()
            } else {
                None
            }
        })
    }
}
