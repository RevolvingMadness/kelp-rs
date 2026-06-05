use kelp_core::parsed::typed_path::{ParsedTypedPath, ParsedTypedPathSegment};

use crate::{
    cst::{CSTGenericDataTypes, CSTTypePath, CSTTypePathSegment},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode, SyntaxTokenExt},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind::{self},
};

impl ParsableAstNode for CSTTypePathSegment {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !parser.try_bump_identifier_kind(SyntaxKind::PathIdentifier) {
            return false;
        }

        marker.start_node(parser, SyntaxKind::TypePathSegment);

        let state = parser.save_state();

        parser.try_bump_str("::", SyntaxKind::ColonColon);

        if !CSTGenericDataTypes::try_parse(parser) {
            state.restore(parser);
        }

        parser.finish_node();

        true
    }
}

impl ParsableAstNode for CSTTypePath {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !CSTTypePathSegment::try_parse(parser) {
            return false;
        }

        marker.start_node(parser, SyntaxKind::TypePath);

        loop {
            let state = parser.save_state();

            parser.skip_inline_whitespace();

            if !parser.try_bump_str("::", SyntaxKind::ColonColon) {
                state.restore(parser);

                break;
            }

            if !CSTTypePathSegment::try_parse(parser) {
                parser.error("Expected path segment");
            }
        }

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTTypePathSegment {
    type Lowered = ParsedTypedPathSegment;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let name_token = self.path_identifier_token()?;
        let name_span = name_token.span();
        let name = name_token.text();

        let (generic_spans, generic_types) = self
            .generic_data_types()
            .and_then(|data_types| data_types.lower(ctx))
            .unwrap_or_default();

        Some(ParsedTypedPathSegment {
            name_span,
            name: name.to_owned(),
            generic_spans,
            generic_types,
        })
    }
}

impl LowerableAstNode for CSTTypePath {
    type Lowered = ParsedTypedPath;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let segments = self
            .type_path_segments()
            .filter_map(|segment| segment.lower(ctx))
            .collect();

        Some(ParsedTypedPath {
            span: self.span(),
            segments,
        })
    }
}
