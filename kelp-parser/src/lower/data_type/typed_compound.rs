use crate::{cst_node, cstlib::token::CSTToken, lower::data_type::CSTDataType, syntax::SyntaxKind};

cst_node!(CSTTypedCompoundField, SyntaxKind::TypedCompoundField);

impl<'a> CSTTypedCompoundField<'a> {
    #[must_use]
    pub fn name_token(&self) -> Option<&'a CSTToken> {
        self.0
            .children_tokens()
            .find(|t| t.kind == SyntaxKind::Identifier)
    }

    #[must_use]
    pub fn data_type(&self) -> Option<CSTDataType<'a>> {
        self.children().find_map(CSTDataType::cast)
    }
}

cst_node!(CSTTypedCompoundDataType, SyntaxKind::TypedCompoundDataType);

impl<'a> CSTTypedCompoundDataType<'a> {
    pub fn fields(&self) -> Vec<CSTTypedCompoundField<'a>> {
        self.0
            .children()
            .filter_map(CSTTypedCompoundField::cast)
            .collect()
    }
}
