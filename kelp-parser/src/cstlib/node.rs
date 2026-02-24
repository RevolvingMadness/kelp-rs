use kelp_core::span::Span;

use crate::{cstlib::CSTNodeType, syntax::SyntaxKind};

#[derive(Debug, Clone)]
pub struct CSTNode {
    pub kind: SyntaxKind,
    pub children: Vec<CSTNodeType>,
    pub span: Span,
}
