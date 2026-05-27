use kelp_core::{
    parsed::supports_expression_sigil::ParsedSupportsExpressionSigil,
    trait_ext::CollectOptionAllIterExt,
};
use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    cst::{
        CSTActualResourceLocation, CSTExpressionSigil, CSTResourceLocation,
        CSTResourceLocationNamespace, CSTResourceLocationPath, CSTResourceLocationPathSegment,
    },
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

fn expect_paths(parser: &mut Parser) {
    parser.start_node(SyntaxKind::ResourceLocationPath);

    parser.start_node(SyntaxKind::ResourceLocationPathSegment);
    parser.expect_identifier("Expected at least one resource location path segment");
    parser.finish_node();

    while parser.try_bump_char('/') {
        parser.start_node(SyntaxKind::ResourceLocationPathSegment);
        parser.expect_identifier("Expected resource location path segment after separator");
        parser.finish_node();
    }

    parser.finish_node();
}

impl ParsableAstNode for CSTResourceLocation {
    fn try_parse(parser: &mut Parser) -> bool {
        if CSTExpressionSigil::try_parse(parser) {
            return true;
        }

        parser.start_node(SyntaxKind::ActualResourceLocation);

        if parser.peek_char() == Some('#') {
            parser.add_token(SyntaxKind::ResourceLocationTag, 1);
        }

        if let Some(namespace) = parser.peek_identifier()
            && parser.peek_nth_char(namespace.len()) == Some(':')
        {
            parser.start_node(SyntaxKind::ResourceLocationNamespace);
            parser.bump_identifier(namespace);
            parser.finish_node();

            parser.bump_char();
        }

        expect_paths(parser);

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTResourceLocationNamespace {
    type Lowered = String;

    fn lower(&self, _ctx: &mut LowerContext) -> Option<Self::Lowered> {
        Some(self.identifier_token()?.text().to_string())
    }
}

impl LowerableAstNode for CSTResourceLocationPathSegment {
    type Lowered = String;

    fn lower(&self, _ctx: &mut LowerContext) -> Option<Self::Lowered> {
        Some(self.identifier_token()?.text().to_string())
    }
}

impl LowerableAstNode for CSTResourceLocationPath {
    type Lowered = Vec<String>;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        self.path_segments()
            .map(|segment| segment.lower(ctx))
            .collect_option_all()
    }
}

impl LowerableAstNode for CSTActualResourceLocation {
    type Lowered = ResourceLocation;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let is_tag = self.pound_token().is_some();

        let namespace = match self.namespace().map(|namespace| namespace.lower(ctx)) {
            None => None,
            Some(None) => return None,
            Some(Some(value)) => Some(value),
        };

        let paths = self.path()?.lower(ctx)?;

        Some(ResourceLocation::new(is_tag, namespace, paths))
    }
}

impl LowerableAstNode for CSTResourceLocation {
    type Lowered = ParsedSupportsExpressionSigil<ResourceLocation>;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        match self {
            Self::ActualResourceLocation(node) => {
                node.lower(ctx).map(ParsedSupportsExpressionSigil::Regular)
            }
            Self::ExpressionSigil(node) => node
                .lower(ctx)
                .map(|lowered| lowered.retype_sigil().unwrap()),
        }
    }
}
