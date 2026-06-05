use std::collections::HashMap;

use kelp_core::{
    parsed::expression::{ParsedExpression, ParsedExpressionKind},
    span::Span,
    trait_ext::CollectOptionAllIterExt,
};

use crate::{
    cst::{
        CSTExpression, CSTStructExpression, CSTStructExpressionField, CSTStructExpressionFields,
    },
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode, SyntaxTokenExt},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind,
};

impl ParsableAstNode for CSTStructExpressionField {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !parser.try_bump_identifier_kind(SyntaxKind::StructFieldName)
            && !parser.try_bump_whole_value()
        {
            return false;
        }

        marker.start_node(parser, SyntaxKind::StructExpressionField);

        parser.skip_whitespace();

        let parsed_colon = parser.expect_char(':');

        parser.skip_whitespace();

        if !CSTExpression::try_parse(parser) && parsed_colon {
            parser.error("Expected expression");
        }

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTStructExpressionField {
    type Lowered = ((Span, String), ParsedExpression);

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let name_token = self.name()?;
        let name_span = name_token.span();
        let name = name_token.text();

        let expression = self.expression()?.lower(ctx)?;

        Some(((name_span, name.to_owned()), expression))
    }
}

impl ParsableAstNode for CSTStructExpressionFields {
    fn try_parse(parser: &mut Parser) -> bool {
        let marker = parser.mark();

        if !CSTStructExpressionField::try_parse(parser) {
            return false;
        }

        marker.start_node(parser, SyntaxKind::StructExpressionFields);

        loop {
            let state = parser.save_state();
            parser.skip_whitespace();

            if !parser.try_bump_char(',') {
                state.restore(parser);

                break;
            }

            parser.skip_whitespace();

            if !CSTStructExpressionField::try_parse(parser) {
                break;
            }
        }

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTStructExpressionFields {
    type Lowered = HashMap<(Span, String), ParsedExpression>;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        self.struct_expression_fields()
            .map(|field| field.lower(ctx))
            .collect_option_all()
    }
}

impl LowerableAstNode for CSTStructExpression {
    type Lowered = ParsedExpression;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let path = self.generic_path()?.lower(ctx)?;

        let fields = self
            .struct_expression_fields()
            .and_then(|fields| fields.lower(ctx))
            .unwrap_or_default();

        Some(ParsedExpressionKind::RegularStruct(path, fields).with_span(self.span()))
    }
}
