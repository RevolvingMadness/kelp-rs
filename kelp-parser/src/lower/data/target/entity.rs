use crate::{cst_node, lower::entity_selector::CSTEntitySelector, syntax::SyntaxKind};

cst_node!(CSTEntityDataTarget, SyntaxKind::EntityDataTarget);

impl<'a> CSTEntityDataTarget<'a> {
    pub fn selector(&self) -> Option<CSTEntitySelector<'a>> {
        self.0.children().find_map(CSTEntitySelector::cast)
    }
}
