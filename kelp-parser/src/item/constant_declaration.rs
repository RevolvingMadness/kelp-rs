use kelp_core::parsed::item::ParsedItemKind;

use crate::{
    cst::{CSTConstantDeclarationItem, CSTDataType, CSTExpression},
    extension_traits::{LowerableAstNode, ParsableAstNode, SyntaxTokenExt},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTConstantDeclarationItem {
    fn try_parse(parser: &mut Parser) -> bool {
        if parser.peek_identifier() != Some("const") {
            return false;
        }

        parser.start_node(SyntaxKind::ConstantDeclarationItem);

        parser.bump_identifier_kind(SyntaxKind::ConstKeyword, "const");

        parser.expect_whitespace();

        if !parser.try_bump_identifier() {
            parser.error("expected constant name");
        }

        parser.skip_whitespace();

        parser.expect_char(':');

        parser.skip_whitespace();

        CSTDataType::expect(parser, "expected data type");

        parser.skip_whitespace();

        parser.expect_char('=');

        parser.skip_whitespace();

        CSTExpression::expect(parser, "expected constant value");

        parser.skip_whitespace();

        parser.expect_char(';');

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTConstantDeclarationItem {
    type Lowered = ParsedItemKind;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let name_token = self.name()?;
        let name_span = name_token.span();
        let name = name_token.text();

        let data_type = self.data_type()?.lower(ctx)?;

        let value = self.expression()?.lower(ctx)?;

        Some(ParsedItemKind::ConstantDeclaration {
            name_span,
            name: name.to_owned(),
            data_type,
            value,
        })
    }
}
