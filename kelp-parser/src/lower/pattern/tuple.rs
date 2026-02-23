use crate::{cst_node, lower::pattern::CSTPattern, syntax::SyntaxKind};

cst_node!(CSTTuplePattern, SyntaxKind::TuplePattern);

impl<'a> CSTTuplePattern<'a> {
    pub(crate) fn patterns(&self) -> impl Iterator<Item = CSTPattern<'a>> {
        self.0.children().filter_map(CSTPattern::cast)
    }
}
