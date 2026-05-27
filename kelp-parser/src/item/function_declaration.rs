use kelp_core::{
    parsed::{
        data_type::ParsedDataType,
        item::{ParsedItemKind, ParsedSelfFunctionParameter},
        pattern::{ParsedPattern, ParsedPatternKind},
    },
    path::generic::GenericPath,
    trait_ext::CollectOptionAllIterExt,
};

use crate::{
    cst::{
        CSTBlockExpression, CSTDataType, CSTFunctionDeclarationItem, CSTFunctionParameter,
        CSTFunctionParameters, CSTPattern, CSTSelfFunctionParameter,
    },
    data_type::generics::{lower_generic_names, try_parse_generic_names},
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode, SyntaxTokenExt},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind::{self},
};

impl ParsableAstNode for CSTFunctionParameter {
    fn try_parse(parser: &mut Parser) -> bool {
        parser.start_node(SyntaxKind::FunctionParameter);

        if !CSTPattern::try_parse(parser) {
            parser.error("Expected pattern");
            parser.finish_node();

            return false;
        }

        parser.skip_whitespace();

        let parsed_colon = parser.expect_char(':', "Expected ':'");

        parser.skip_whitespace();

        if !CSTDataType::try_parse(parser) && parsed_colon {
            parser.error("Expected data type");
        }

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTFunctionParameter {
    type Lowered = (ParsedPattern, ParsedDataType);

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let pattern = self.pattern()?.lower(ctx)?;

        let data_type = self.data_type()?.lower(ctx)?;

        Some((pattern, data_type))
    }
}

impl ParsableAstNode for CSTSelfFunctionParameter {
    fn try_parse(parser: &mut Parser) -> bool {
        if parser.peek_identifier() != Some("self") {
            return false;
        }

        parser.start_node(SyntaxKind::SelfFunctionParameter);
        parser.bump_str(SyntaxKind::SelfKeyword, "self");

        parser.skip_whitespace();

        if parser.try_bump_char(':') {
            parser.skip_whitespace();

            if !CSTDataType::try_parse(parser) {
                parser.error("Expected data type");
            }
        }

        parser.finish_node();
        true
    }
}

impl ParsableAstNode for CSTFunctionParameters {
    fn try_parse(parser: &mut Parser) -> bool {
        parser.start_node(SyntaxKind::FunctionParameters);

        if !parser.expect_char('(', "Expected '('") {
            parser.finish_node();
            return false;
        }

        parser.skip_whitespace();

        if parser.peek_char() != Some(')') {
            if CSTSelfFunctionParameter::try_parse(parser) {
                parser.skip_whitespace();

                if parser.try_bump_char(',') {
                    parser.skip_whitespace();
                }
            }

            if parser.peek_char() != Some(')') {
                loop {
                    if !CSTFunctionParameter::try_parse(parser) {
                        break;
                    }

                    parser.skip_whitespace();

                    if !parser.try_bump_char(',') {
                        break;
                    }

                    parser.skip_whitespace();

                    if parser.peek_char() == Some(')') {
                        break;
                    }
                }
            }
        }

        parser.expect_char(')', "Expected ')'");
        parser.finish_node();

        true
    }
}

impl ParsableAstNode for CSTFunctionDeclarationItem {
    fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::FunctionDeclarationItem);

        let mut is_recursive = false;

        if parser.peek_identifier() == Some("recursive") {
            parser.bump_str(SyntaxKind::RecursiveKeyword, "recursive");

            if !parser.expect_whitespace() {
                state.restore(parser);
                return false;
            }

            is_recursive = true;
        }

        if parser.peek_identifier() == Some("runtime") {
            parser.bump_str(SyntaxKind::RuntimeKeyword, "runtime");

            if !parser.expect_whitespace() && !is_recursive {
                state.restore(parser);
                return false;
            }
        }

        parser.bump_str(SyntaxKind::FNKeyword, "fn");

        if !parser.expect_whitespace() || !parser.try_bump_identifier_kind(SyntaxKind::FunctionName)
        {
            state.restore(parser);
            return false;
        }

        parser.skip_whitespace();

        if try_parse_generic_names(parser) {
            parser.skip_whitespace();
        }

        let _ = CSTFunctionParameters::try_parse(parser);

        parser.skip_whitespace();

        if parser.try_bump_str("->", SyntaxKind::MinusRightArrow) {
            parser.skip_whitespace();

            if !CSTDataType::try_parse(parser) {
                parser.error("Expected function return type");
            }

            parser.skip_whitespace();
        }

        CSTBlockExpression::expect(parser, "Expected function body");

        parser.finish_node();

        true
    }
}

impl LowerableAstNode for CSTFunctionParameters {
    type Lowered = (
        Option<ParsedSelfFunctionParameter>,
        Vec<(ParsedPattern, ParsedDataType)>,
    );

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let self_parameter = self
            .self_function_parameters()
            .next()
            .and_then(|parameter| {
                let self_keyword_span = parameter.self_token()?.span();

                let self_type = parameter.data_type().and_then(|data_type| {
                    let span = data_type.span();

                    let data_type = data_type.lower(ctx)?;

                    Some((span, data_type))
                });

                Some(ParsedSelfFunctionParameter {
                    span: self_keyword_span,
                    data_type: self_type,
                })
            });

        let parameters = self
            .parameters()
            .map(|parameter| parameter.lower(ctx))
            .collect_option_all()?;

        Some((self_parameter, parameters))
    }
}

impl LowerableAstNode for CSTFunctionDeclarationItem {
    type Lowered = ParsedItemKind;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let recursive_keyword_span = self.recursive_token().map(|token| token.span());

        let runtime_keyword_span = self.runtime_token().map(|token| token.span());

        let name_token = self.name()?;
        let name_span = name_token.span();
        let name = name_token.text();

        let generic_names = self.generic_names().and_then(lower_generic_names);

        let parameters = self
            .function_parameters()
            .map(|parameters| parameters.lower(ctx));

        let return_type = self
            .data_type()
            .and_then(|data_type| data_type.lower(ctx))
            .unwrap_or(ParsedDataType::Unit);

        let body = self.block_expression()?.lower(ctx)?;

        let (self_parameter, mut parameters) = match parameters {
            Some(Some(value)) => value,
            Some(None) => return None,
            None => (None, Vec::new()),
        };

        if let Some(self_parameter) = &self_parameter {
            let self_pattern = ParsedPattern {
                span: self_parameter.span,
                kind: ParsedPatternKind::Binding(GenericPath::single(self_parameter.span, "self")),
            };

            let self_type = ParsedDataType::Named(GenericPath::single(
                self_parameter
                    .data_type
                    .as_ref()
                    .map_or(self_parameter.span, |(span, _)| *span),
                "Self",
            ));

            parameters.insert(0, (self_pattern, self_type));
        }

        Some(ParsedItemKind::FunctionDeclaration {
            recursive_keyword_span,
            runtime_keyword_span,
            name_span,
            name: name.to_owned(),
            generic_names: generic_names.unwrap_or_default(),
            self_parameter,
            parameters,
            return_type,
            body: Box::new(body),
        })
    }
}
