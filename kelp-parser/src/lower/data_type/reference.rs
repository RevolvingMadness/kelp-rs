use crate::{cst_node, lower::data_type::CSTDataType, syntax::SyntaxKind};

cst_node!(CSTReferenceDataType, SyntaxKind::ReferenceDataType);

impl<'a> CSTReferenceDataType<'a> {
    pub fn inner(&self) -> Option<CSTDataType<'a>> {
        self.0.children().find_map(CSTDataType::cast)
    }
}
