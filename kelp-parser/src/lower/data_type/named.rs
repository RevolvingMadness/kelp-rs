use crate::{cst_node, cstlib::token::CSTToken, lower::data_type::CSTDataType, syntax::SyntaxKind};

cst_node!(CSTNamedDataType, SyntaxKind::NamedDataType);

impl<'a> CSTNamedDataType<'a> {
    pub fn name_token(&self) -> Option<&'a CSTToken> {
        self.0
            .children_tokens()
            .find(|t| t.kind == SyntaxKind::Identifier)
    }

    pub fn generics(&self) -> Vec<CSTDataType<'a>> {
        self.0.children().filter_map(CSTDataType::cast).collect()
    }
}
