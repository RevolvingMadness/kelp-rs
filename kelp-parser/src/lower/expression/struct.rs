use kelp_core::span::Span;

use crate::{
    cst_node,
    lower::{data_type::CSTDataType, expression::CSTExpression},
    parser::Parser,
    syntax::SyntaxKind,
};

cst_node!(CSTStructExpressionField, SyntaxKind::StructExpressionField);

impl<'a> CSTStructExpressionField<'a> {
    pub fn name<'b>(&self, text: &'b str) -> Option<(Span, &'b str)> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some((token.span, token.text(text)))
            } else {
                None
            }
        })
    }

    pub fn value(&self) -> Option<CSTExpression<'a>> {
        self.children().find_map(CSTExpression::cast)
    }
}

cst_node!(CSTStructExpression, SyntaxKind::StructExpression);

impl<'a> CSTStructExpression<'a> {
    pub fn bump_until_next_field_or_end(parser: &mut Parser) {
        let chars = parser.source[parser.pos..].chars();
        let mut length = 0;

        for char in chars {
            if char == ',' || char == '}' || char.is_alphabetic() {
                break;
            }

            length += char.len_utf8();
        }

        if length > 0 {
            parser.add_token(SyntaxKind::Garbage, length);
        }

        parser.try_bump_char(',');
    }

    pub fn name<'b>(&self, text: &'b str) -> Option<(Span, &'b str)> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some((token.span, token.text(text)))
            } else {
                None
            }
        })
    }

    pub fn generics(&self) -> Vec<CSTDataType<'a>> {
        self.children().filter_map(CSTDataType::cast).collect()
    }

    pub fn fields(&self) -> impl Iterator<Item = CSTStructExpressionField<'a>> {
        self.children().filter_map(CSTStructExpressionField::cast)
    }
}
