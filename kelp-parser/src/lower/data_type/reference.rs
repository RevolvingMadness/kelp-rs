use crate::{cst_node, lower::data_type::CSTDataType, syntax::SyntaxKind};

cst_node!(CSTReferenceDataType, SyntaxKind::ReferenceDataType);

impl<'a> CSTReferenceDataType<'a> {
    #[must_use]
    pub fn inner(&self) -> Option<CSTDataType<'a>> {
        self.children().find_map(CSTDataType::cast)
    }
}
