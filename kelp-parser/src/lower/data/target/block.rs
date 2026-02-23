use crate::{cst_node, lower::coordinates::CSTCoordinates, syntax::SyntaxKind};

cst_node!(CSTBlockDataTarget, SyntaxKind::BlockDataTarget);

impl<'a> CSTBlockDataTarget<'a> {
    pub fn coordinates(&self) -> Option<CSTCoordinates<'a>> {
        self.0.children().find_map(CSTCoordinates::cast)
    }
}
