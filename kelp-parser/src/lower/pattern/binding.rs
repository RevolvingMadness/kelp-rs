use crate::{cst_node, syntax::SyntaxKind};

cst_node!(CSTBindingPattern, SyntaxKind::BindingPattern);

impl<'a> CSTBindingPattern<'a> {
    pub fn name(&self) -> Option<&'a str> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some(token.text)
            } else {
                None
            }
        })
    }
}
