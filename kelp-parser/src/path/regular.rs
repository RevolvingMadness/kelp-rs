use kelp_core::{
    path::regular::{Path, PathSegment},
    trait_ext::CollectOptionAllIterExt,
};

use crate::{
    cst::{CSTPath, CSTPathSegment},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTPathSegment {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !parser.try_bump_identifier_kind(SyntaxKind::PathIdentifier) {
            return false;
        }

        marker.start_node(parser, SyntaxKind::PathSegment);

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTPathSegment {
    type Lowered = PathSegment;

    fn lower(&self, _ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let name_token = self.path_identifier_token()?;
        let name = name_token.text();

        Some(PathSegment {
            span: self.span(),
            name: name.to_owned(),
        })
    }
}

impl ParsableAstNode for CSTPath {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !CSTPathSegment::try_parse(parser) {
            return false;
        }

        marker.start_node(parser, SyntaxKind::Path);

        loop {
            let state = parser.save_state();

            parser.skip_inline_whitespace();

            if !parser.try_bump_str("::", SyntaxKind::ColonColon)
                || !CSTPathSegment::try_parse(parser)
            {
                state.restore(parser);

                break;
            }
        }

        parser.finish_node();
        true
    }
}

impl LowerableAstNode for CSTPath {
    type Lowered = Path;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let segments = self
            .path_segments()
            .map(|segment| segment.lower(ctx))
            .collect_option_all()?;

        Some(Path {
            span: self.span(),
            segments,
        })
    }
}
