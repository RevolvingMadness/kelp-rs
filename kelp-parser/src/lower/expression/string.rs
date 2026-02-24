use crate::{cst_node, syntax::SyntaxKind};

cst_node!(CSTStringExpression, SyntaxKind::StringExpression);

impl<'a> CSTStringExpression<'a> {
    pub fn value<'b>(&self, text: &'b str) -> Option<&'b str> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::String {
                Some(token.text(text).trim_matches('"'))
            } else {
                None
            }
        })
    }
}
