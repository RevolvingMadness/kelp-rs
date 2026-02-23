use crate::{cst_node, syntax::SyntaxKind};

cst_node!(CSTVariableExpression, SyntaxKind::VariableExpression);

impl<'a> CSTVariableExpression<'a> {
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
