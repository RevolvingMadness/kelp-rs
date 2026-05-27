use kelp_core::{parsed::data_type::ParsedDataType, span::Span};

use crate::{
    cst::{CSTDataType, CSTGenericDataTypes, CSTGenericNames},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTGenericNames {
    fn try_parse(parser: &mut Parser) -> bool {
        if parser.peek_char() != Some('<') {
            return false;
        }

        parser.start_node(SyntaxKind::GenericNames);

        parser.bump_char();
        parser.skip_inline_whitespace();

        while parser.peek_char() != Some('>') && parser.peek_char().is_some() {
            if !parser
                .expect_identifier_kind(SyntaxKind::DataTypeName, "Expected generic argument name")
            {
                break;
            }

            parser.skip_inline_whitespace();

            if parser.try_bump_char(',') {
                parser.skip_inline_whitespace();
            } else {
                break;
            }
        }

        parser.expect_char('>', "Expected closing angle bracket '>'");

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTGenericNames {
    type Lowered = Vec<String>;

    fn lower(&self, _ctx: &mut LowerContext) -> Option<Self::Lowered> {
        Some(
            self.generics()
                .map(|token| token.text().to_owned())
                .collect(),
        )
    }
}

impl ParsableAstNode for CSTGenericDataTypes {
    fn try_parse(parser: &mut Parser) -> bool {
        if parser.peek_char() != Some('<') {
            return false;
        }

        parser.start_node(SyntaxKind::GenericDataTypes);

        parser.bump_char();
        parser.skip_inline_whitespace();

        while parser.peek_char() != Some('>') && parser.peek_char().is_some() {
            if !CSTDataType::expect(parser, "Expected data type in generic arguments") {
                break;
            }

            parser.skip_inline_whitespace();

            if parser.try_bump_char(',') {
                parser.skip_inline_whitespace();
            } else {
                break;
            }
        }

        parser.expect_char('>', "Expected closing angle bracket '>'");

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTGenericDataTypes {
    type Lowered = (Vec<Span>, Vec<ParsedDataType>);

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        Some(
            self.generics()
                .filter_map(|data_type| {
                    let span = data_type.span();
                    let data_type = data_type.lower(ctx)?;

                    Some((span, data_type))
                })
                .unzip(),
        )
    }
}
