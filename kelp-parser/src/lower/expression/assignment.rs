use crate::{cst_node, lower::expression::CSTExpression, syntax::SyntaxKind};

cst_node!(CSTAssignmentExpression, SyntaxKind::AssignmentExpression);

impl<'a> CSTAssignmentExpression<'a> {
    #[must_use]
    pub fn lhs(&self) -> Option<CSTExpression<'a>> {
        self.children().find_map(CSTExpression::cast)
    }

    #[must_use]
    pub fn rhs(&self) -> Option<CSTExpression<'a>> {
        self.children().filter_map(CSTExpression::cast).nth(1)
    }

    #[must_use]
    pub fn op_kind(&self) -> Option<SyntaxKind> {
        self.0.children_tokens().find_map(|t| {
            matches!(
                t.kind,
                SyntaxKind::Equal
                    | SyntaxKind::PlusEqual
                    | SyntaxKind::MinusEqual
                    | SyntaxKind::StarEqual
                    | SyntaxKind::ForwardSlashEqual
                    | SyntaxKind::PercentEqual
            )
            .then_some(t.kind)
        })
    }
}
