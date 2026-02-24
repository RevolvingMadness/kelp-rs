use crate::{cst_node, cstlib::CSTNodeType, lower::expression::CSTExpression, syntax::SyntaxKind};

cst_node!(CSTBinaryExpression, SyntaxKind::BinaryExpression);

impl<'a> CSTBinaryExpression<'a> {
    #[must_use]
    pub fn lhs(&self) -> Option<CSTExpression<'a>> {
        self.children().find_map(CSTExpression::cast)
    }

    #[must_use]
    pub fn rhs(&self) -> Option<CSTExpression<'a>> {
        self.children().filter_map(CSTExpression::cast).nth(1)
    }

    #[must_use]
    pub fn op_details<'b>(&self, text: &'b str) -> Option<(&'b str, SyntaxKind)> {
        self.children().find_map(|child| {
            if let CSTNodeType::Token(token) = child
                && matches!(
                    token.kind,
                    SyntaxKind::Plus
                        | SyntaxKind::Minus
                        | SyntaxKind::Star
                        | SyntaxKind::ForwardSlash
                        | SyntaxKind::Percent
                        | SyntaxKind::RightArrow
                        | SyntaxKind::RightArrowEqual
                        | SyntaxKind::LeftArrow
                        | SyntaxKind::LeftArrowEqual
                        | SyntaxKind::EqualEqual
                        | SyntaxKind::ExclamationMarkEqual
                        | SyntaxKind::AmpersandAmpersand
                        | SyntaxKind::PipePipe
                        | SyntaxKind::Ampersand
                        | SyntaxKind::Pipe
                        | SyntaxKind::LeftArrowLeftArrow
                        | SyntaxKind::RightArrowRightArrow
                )
            {
                return Some((token.text(text), token.kind));
            }
            None
        })
    }
}
