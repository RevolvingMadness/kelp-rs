use kelp_core::span::Span;

use crate::{
    cst_node,
    lower::{data_type::CSTDataType, struct_field::CSTStructField},
    syntax::SyntaxKind,
};

cst_node!(CSTStructExpression, SyntaxKind::StructExpression);

impl<'a> CSTStructExpression<'a> {
    pub fn name(&self) -> Option<(Span, &'a str)> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some((token.span, token.text))
            } else {
                None
            }
        })
    }

    pub fn generics(&self) -> Vec<CSTDataType<'a>> {
        self.0.children().filter_map(CSTDataType::cast).collect()
    }

    pub fn fields(&self) -> impl Iterator<Item = CSTStructField<'a>> {
        self.0.children().filter_map(CSTStructField::cast)
    }
}
