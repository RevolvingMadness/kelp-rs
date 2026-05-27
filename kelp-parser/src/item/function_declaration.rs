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
    cst::{CSTFunctionDeclarationItem, CSTFunctionParameter, CSTFunctionParameters},
    data_type::{
        generics::{lower_generic_names, try_parse_generic_names},
        lower_data_type, try_parse_data_type,
    },
    expression::with_block::block::{lower_block_expression, try_parse_block_expression},
    lower_context::LowerContext,
    parser::Parser,
    pattern::{lower_pattern, try_parse_pattern},
    span::{span_of_cst_node, text_range_to_span},
    syntax::SyntaxKind::{self},
};

#[must_use]
pub fn try_parse_function_parameter(parser: &mut Parser) -> bool {
    parser.start_node(SyntaxKind::FunctionParameter);

    if !try_parse_pattern(parser) {
        parser.error("Expected pattern");
        parser.finish_node();

        return false;
    }

    parser.skip_whitespace();

    let parsed_colon = parser.expect_char(':', "Expected ':'");

    parser.skip_whitespace();

    if !try_parse_data_type(parser) && parsed_colon {
        parser.error("Expected data type");
    }

    parser.finish_node();

    true
}

#[must_use]
pub fn try_parse_self_function_parameter(parser: &mut Parser) -> bool {
    if parser.peek_identifier() != Some("self") {
        return false;
    }

    parser.start_node(SyntaxKind::SelfFunctionParameter);
    parser.bump_str(SyntaxKind::SelfKeyword, "self");

    parser.skip_whitespace();

    if parser.try_bump_char(':') {
        parser.skip_whitespace();

        if !try_parse_data_type(parser) {
            parser.error("Expected data type");
        }
    }

    parser.finish_node();
    true
}

#[must_use]
pub fn try_parse_function_parameters(parser: &mut Parser) -> bool {
    parser.start_node(SyntaxKind::FunctionParameters);

    if !parser.expect_char('(', "Expected '('") {
        parser.finish_node();
        return false;
    }

    parser.skip_whitespace();

    if parser.peek_char() != Some(')') {
        if try_parse_self_function_parameter(parser) {
            parser.skip_whitespace();

            if parser.try_bump_char(',') {
                parser.skip_whitespace();
            }
        }

        if parser.peek_char() != Some(')') {
            loop {
                if !try_parse_function_parameter(parser) {
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

#[must_use]
pub fn try_parse_function_declaration_item_kind(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::FunctionDeclarationItem);

    let mut is_recursive = false;

    if parser.peek_identifier() == Some("recursive") {
        parser.bump_str(SyntaxKind::RecursiveKeyword, "recursive");

        if !parser.expect_whitespace() {
            parser.restore_state(state);
            return false;
        }

        is_recursive = true;
    }

    if parser.peek_identifier() == Some("runtime") {
        parser.bump_str(SyntaxKind::RuntimeKeyword, "runtime");

        if !parser.expect_whitespace() && !is_recursive {
            parser.restore_state(state);
            return false;
        }
    }

    parser.bump_str(SyntaxKind::FNKeyword, "fn");

    if !parser.expect_whitespace() || !parser.try_bump_identifier_kind(SyntaxKind::FunctionName) {
        parser.restore_state(state);
        return false;
    }

    parser.skip_whitespace();

    if try_parse_generic_names(parser) {
        parser.skip_whitespace();
    }

    let _ = try_parse_function_parameters(parser);

    parser.skip_whitespace();

    if parser.try_bump_str("->", SyntaxKind::MinusRightArrow) {
        parser.skip_whitespace();

        if !try_parse_data_type(parser) {
            parser.error("Expected function return type");
        }

        parser.skip_whitespace();
    }

    if !try_parse_block_expression(parser) {
        parser.error("Expected block expression");
    }

    parser.finish_node();

    true
}

pub fn expect_function_declaration_item_kind(parser: &mut Parser) {
    parser.start_node(SyntaxKind::FunctionDeclarationItem);

    if parser.peek_identifier() == Some("recursive") {
        parser.bump_str(SyntaxKind::RecursiveKeyword, "recursive");
        parser.expect_whitespace();
    }

    if parser.peek_identifier() == Some("runtime") {
        parser.bump_str(SyntaxKind::RuntimeKeyword, "runtime");
        parser.expect_whitespace();
    }

    parser.bump_str(SyntaxKind::FNKeyword, "fn");
    parser.expect_whitespace();

    if !parser.try_bump_identifier_kind(SyntaxKind::FunctionName) {
        parser.error("Expected function name");
    }

    parser.skip_whitespace();

    if try_parse_generic_names(parser) {
        parser.skip_whitespace();
    }

    let _ = try_parse_function_parameters(parser);

    parser.skip_whitespace();

    if parser.try_bump_str("->", SyntaxKind::MinusRightArrow) {
        parser.skip_whitespace();

        if !try_parse_data_type(parser) {
            parser.error("Expected function return type");
        }

        parser.skip_whitespace();
    }

    if !try_parse_block_expression(parser) {
        parser.error("Expected block expression");
    }

    parser.finish_node();
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_function_parameter(
    node: CSTFunctionParameter,
    ctx: &mut LowerContext,
) -> Option<(ParsedPattern, ParsedDataType)> {
    let pattern = lower_pattern(node.pattern()?, ctx)?;

    let data_type = lower_data_type(node.data_type()?)?;

    Some((pattern, data_type))
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
#[allow(clippy::type_complexity)]
pub fn lower_function_parameters(
    node: CSTFunctionParameters,
    ctx: &mut LowerContext,
) -> Option<(
    Option<ParsedSelfFunctionParameter>,
    Vec<(ParsedPattern, ParsedDataType)>,
)> {
    let self_parameter = node
        .self_function_parameters()
        .next()
        .and_then(|parameter| {
            let self_keyword_span = text_range_to_span(parameter.self_token()?.text_range());

            let self_type = parameter.data_type().and_then(|data_type| {
                let span = span_of_cst_node(&data_type);

                let data_type = lower_data_type(data_type)?;

                Some((span, data_type))
            });

            Some(ParsedSelfFunctionParameter {
                span: self_keyword_span,
                data_type: self_type,
            })
        });

    let parameters = node
        .parameters()
        .map(|parameter| lower_function_parameter(parameter, ctx))
        .collect_option_all()?;

    Some((self_parameter, parameters))
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_function_declaration_item_kind(
    node: CSTFunctionDeclarationItem,
    ctx: &mut LowerContext,
) -> Option<ParsedItemKind> {
    let recursive_keyword_span = node
        .recursive_token()
        .map(|token| text_range_to_span(token.text_range()));

    let runtime_keyword_span = node
        .runtime_token()
        .map(|token| text_range_to_span(token.text_range()));

    let name_token = node.name()?;
    let name_span = name_token.text_range();
    let name = name_token.text();

    let generic_names = node.generic_names().and_then(lower_generic_names);

    let parameters = node
        .function_parameters()
        .map(|parameters| lower_function_parameters(parameters, ctx));

    let return_type = node
        .data_type()
        .and_then(lower_data_type)
        .unwrap_or(ParsedDataType::Unit);

    let body = lower_block_expression(node.block_expression()?, ctx)?;

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
        name_span: text_range_to_span(name_span),
        name: name.to_owned(),
        generic_names: generic_names.unwrap_or_default(),
        self_parameter,
        parameters,
        return_type,
        body: Box::new(body),
    })
}
