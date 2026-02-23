use crate::{cst_node, syntax::SyntaxKind};

cst_node!(CSTStringExpression, SyntaxKind::StringExpression);

impl<'a> CSTStringExpression<'a> {
    pub fn value(&self) -> Option<&'a str> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::String {
                Some(token.text.trim_matches('"'))
            } else {
                None
            }
        })
    }
}
