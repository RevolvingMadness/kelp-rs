use kelp_core::{
    parsed::data_type::ParsedDataType,
    path::generic::{GenericPath, GenericPathSegment},
};

use crate::{
    cst::{CSTGenericDataTypes, CSTGenericPath, CSTGenericPathSegment},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode, SyntaxTokenExt},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind::{self},
};

impl ParsableAstNode for CSTGenericPathSegment {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !parser.try_bump_identifier_kind(SyntaxKind::PathIdentifier) {
            return false;
        }

        marker.start_node(parser, SyntaxKind::GenericPathSegment);

        let state = parser.save_state();

        if parser.try_bump_str("::", SyntaxKind::ColonColon)
            && !CSTGenericDataTypes::try_parse(parser)
        {
            state.restore(parser);
        }

        parser.finish_node();

        true
    }
}

impl ParsableAstNode for CSTGenericPath {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !CSTGenericPathSegment::try_parse(parser) {
            return false;
        }

        marker.start_node(parser, SyntaxKind::GenericPath);

        loop {
            let state = parser.save_state();

            parser.skip_inline_whitespace();

            if !parser.try_bump_str("::", SyntaxKind::ColonColon) {
                state.restore(parser);

                break;
            }

            if !CSTGenericPathSegment::try_parse(parser) {
                parser.error("Expected path segment");
            }
        }

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTGenericPathSegment {
    type Lowered = GenericPathSegment<ParsedDataType>;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let name_token = self.path_identifier_token()?;
        let name_span = name_token.span();
        let name = name_token.text();

        let (generic_spans, generic_types) = self
            .generic_data_types()
            .and_then(|data_types| data_types.lower(ctx))
            .unwrap_or_default();

        Some(GenericPathSegment {
            name_span,
            name: name.to_owned(),
            generic_spans,
            generic_types,
        })
    }
}

impl LowerableAstNode for CSTGenericPath {
    type Lowered = GenericPath<ParsedDataType>;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let segments = self
            .generic_path_segments()
            .filter_map(|segment| segment.lower(ctx))
            .collect();

        Some(GenericPath {
            span: self.span(),
            segments,
        })
    }
}
