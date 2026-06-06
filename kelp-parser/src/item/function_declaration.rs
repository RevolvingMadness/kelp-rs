use kelp_core::{
    parsed::{
        data_type::ParsedDataType,
        item::{FunctionQualifiers, ParsedItemKind, ParsedSelfFunctionParameter},
        pattern::ParsedPattern,
        typed_path::ParsedTypedPath,
    },
    trait_ext::CollectOptionAllIterExt,
};

use crate::{
    cst::{
        CSTBlockExpression, CSTDataType, CSTFunctionDeclarationItem, CSTFunctionParameter,
        CSTFunctionParameters, CSTFunctionQualifiers, CSTGenericNames, CSTPattern,
        CSTSelfFunctionParameter,
    },
    extension_traits::{AstNodeExt, LowerableAstNode, ParsableAstNode, SyntaxTokenExt},
    lower_context::LowerContext,
    parser::Parser,
    syntax::SyntaxKind::{self},
};

impl ParsableAstNode for CSTFunctionQualifiers {
    fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        let mut parsed = false;

        parser.start_node(SyntaxKind::FunctionQualifiers);

        if parser.peek_identifier() == Some("recursive") {
            parser.bump_identifier_kind(SyntaxKind::RecursiveKeyword, "recursive");

            parser.skip_whitespace();

            parsed = true;
        }

        if parser.peek_identifier() == Some("runtime") {
            parser.bump_identifier_kind(SyntaxKind::RuntimeKeyword, "runtime");

            parser.skip_whitespace();

            parsed = true;
        }

        if parser.peek_identifier() == Some("const") {
            parser.bump_identifier_kind(SyntaxKind::ConstKeyword, "const");

            parser.skip_whitespace();

            parsed = true;
        }

        parser.finish_node();

        if parsed {
            true
        } else {
            state.restore(parser);

            false
        }
    }
}

impl LowerableAstNode for CSTFunctionQualifiers {
    type Lowered = FunctionQualifiers;

    fn lower(&self, _ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let recursive_span = self.recursive_token().map(|token| token.span());

        let runtime_span = self.runtime_token().map(|token| token.span());

        let constant_span = self.const_token().map(|token| token.span());

        Some(FunctionQualifiers {
            recursive: recursive_span,
            runtime: runtime_span,
            constant: constant_span,
        })
    }
}

impl ParsableAstNode for CSTFunctionParameter {
    fn try_parse(parser: &mut Parser) -> bool {
        parser.start_node(SyntaxKind::FunctionParameter);

        if !CSTPattern::try_parse(parser) {
            parser.error("Expected pattern");
            parser.finish_node();

            return false;
        }

        parser.skip_whitespace();

        let parsed_colon = parser.expect_char(':');

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

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
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

            CSTDataType::expect(parser, "Expected data type");
        }

        parser.finish_node();

        true
    }
}

impl ParsableAstNode for CSTFunctionParameters {
    fn try_parse(parser: &mut Parser) -> bool {
        parser.start_node(SyntaxKind::FunctionParameters);

        if !parser.expect_char('(') {
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

        parser.expect_char(')');
        parser.finish_node();

        true
    }
}

impl ParsableAstNode for CSTFunctionDeclarationItem {
    fn try_parse(parser: &mut Parser) -> bool {
        let state = parser.save_state();

        parser.start_node(SyntaxKind::FunctionDeclarationItem);

        CSTFunctionQualifiers::try_parse(parser);

        if !parser.try_bump_str("fn", SyntaxKind::FNKeyword) {
            state.restore(parser);

            return false;
        }

        parser.expect_whitespace();

        if !parser.try_bump_identifier_kind(SyntaxKind::FunctionName) {
            parser.error("Expected function name");
        }

        parser.skip_whitespace();

        if CSTGenericNames::try_parse(parser) {
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

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let self_parameter = self
            .self_function_parameters()
            .next()
            .and_then(|parameter| {
                let self_keyword_span = parameter.self_token()?.span();

                let (self_type_span, self_type) = parameter
                    .data_type()
                    .and_then(|data_type| {
                        let span = data_type.span();

                        let data_type = data_type.lower(ctx)?;

                        Some((span, data_type))
                    })
                    .unwrap_or_else(|| {
                        (
                            self_keyword_span,
                            ParsedDataType::Named(ParsedTypedPath::single(
                                self_keyword_span,
                                "Self",
                            )),
                        )
                    });

                Some(ParsedSelfFunctionParameter {
                    pattern_span: self_keyword_span,
                    data_type_span: self_type_span,
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

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let qualifiers = self.function_qualifiers()?.lower(ctx)?;

        let name_token = self.name()?;
        let name_span = name_token.span();
        let name = name_token.text();

        let generics = self.generic_names().and_then(|names| names.lower(ctx));

        let parameters = self
            .function_parameters()
            .map(|parameters| parameters.lower(ctx));

        let return_type = self
            .data_type()
            .and_then(|data_type| data_type.lower(ctx))
            .unwrap_or(ParsedDataType::Unit);

        let body = self.block_expression()?.lower(ctx)?;

        let (self_parameter, parameters) = match parameters {
            Some(Some(value)) => value,
            Some(None) => return None,
            None => (None, Vec::new()),
        };

        Some(ParsedItemKind::FunctionDeclaration {
            qualifiers,
            name_span,
            name: name.to_owned(),
            generics: generics.unwrap_or_default(),
            self_parameter,
            parameters,
            return_type,
            body: Box::new(body),
        })
    }
}
