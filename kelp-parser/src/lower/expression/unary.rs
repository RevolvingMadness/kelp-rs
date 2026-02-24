use crate::{cst_node, lower::expression::CSTExpression, syntax::SyntaxKind};

cst_node!(CSTUnaryExpression, SyntaxKind::UnaryExpression);

impl<'a> CSTUnaryExpression<'a> {
    #[must_use]
    pub fn operand(&self) -> Option<CSTExpression<'a>> {
        self.children().find_map(CSTExpression::cast)
    }

    #[must_use]
    pub fn op_kind(&self) -> Option<SyntaxKind> {
        self.0.children_tokens().find_map(|t| {
            matches!(
                t.kind,
                SyntaxKind::ExclamationMark
                    | SyntaxKind::Minus
                    | SyntaxKind::Star
                    | SyntaxKind::Ampersand
            )
            .then_some(t.kind)
        })
    }
}
