use crate::{
    cst_node,
    lower::{data_type::CSTDataType, expression::CSTExpression},
    syntax::SyntaxKind,
};

cst_node!(CSTAsCastExpression, SyntaxKind::AsCastExpression);

impl<'a> CSTAsCastExpression<'a> {
    pub fn expression(&self) -> Option<CSTExpression<'a>> {
        self.0.children().find_map(CSTExpression::cast)
    }

    pub fn data_type(&self) -> Option<CSTDataType<'a>> {
        self.0.children().find_map(CSTDataType::cast)
    }
}
