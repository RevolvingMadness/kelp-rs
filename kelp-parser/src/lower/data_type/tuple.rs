use crate::{cst_node, lower::data_type::CSTDataType, syntax::SyntaxKind};

cst_node!(CSTTupleDataType, SyntaxKind::TupleDataType);

impl<'a> CSTTupleDataType<'a> {
    pub fn data_types(&self) -> Vec<CSTDataType<'a>> {
        self.children().filter_map(CSTDataType::cast).collect()
    }
}
