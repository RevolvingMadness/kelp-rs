use crate::{cst_node, lower::resource_location::CSTResourceLocation, syntax::SyntaxKind};

cst_node!(CSTStorageDataTarget, SyntaxKind::StorageDataTarget);

impl<'a> CSTStorageDataTarget<'a> {
    pub fn resource_location(&self) -> Option<CSTResourceLocation<'a>> {
        self.0.children().find_map(CSTResourceLocation::cast)
    }
}
