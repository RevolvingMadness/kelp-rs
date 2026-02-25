use kelp_core::{high::entity_selector::HighEntitySelector, span::Span};

use crate::{
    cstlib::CSTNodeType,
    lower::entity_selector::variable::CSTVariableEntitySelector,
    parser::Parser,
    semantic_token::{SemanticToken, SemanticTokenType},
    syntax::SyntaxKind,
};

pub mod variable;

pub enum CSTEntitySelector<'a> {
    Variable(CSTVariableEntitySelector<'a>),
    Name(Span),
}

impl<'a> CSTEntitySelector<'a> {
    pub fn try_parse(parser: &mut Parser) -> bool {
        if parser.try_start_node_bump('@', SyntaxKind::VariableEntitySelector) {
            if let Some(text) = parser.peek_identifier() {
                parser.add_token(SyntaxKind::EntitySelectorVariable, text.len());
            } else {
                parser.error("Expected entity selector variable");
            }

            if parser.peek_char() == Some('[') {
                CSTEntitySelector::parse_options(parser);
            }

            parser.finish_node();
        } else if let Some(name) = parser.peek_identifier() {
            parser.add_token(SyntaxKind::EntitySelectorName, name.len());
        } else {
            parser.error("Expected entity selector");

            return false;
        }

        true
    }

    #[must_use]
    fn parse_option(parser: &mut Parser) -> bool {
        parser.start_node(SyntaxKind::VariableEntitySelectorOption);

        let Some(key_name) = parser.peek_identifier() else {
            parser.error("Expected selector option name");
            CSTEntitySelector::recover_option(parser);
            parser.finish_node();

            return false;
        };

        let key_string = key_name.to_string();
        let name_position = parser.pos;
        parser.bump_identifier(key_name);
        parser.skip_whitespace();

        if !parser.expect_char('=', "Expected '=' after option name") {
            CSTEntitySelector::recover_option(parser);
            parser.finish_node();
        }

        parser.skip_whitespace();

        let is_negated = parser.try_bump_char('!');
        if is_negated {
            parser.skip_whitespace();
        }

        parser.start_node(SyntaxKind::VariableEntitySelectorOptionValue);

        match key_string.as_str() {
            "x" | "y" | "z" | "dx" | "dy" | "dz" | "x_rotation" | "y_rotation" => {
                parser.expect_fractional_value("Expected float value");
            }

            "name" | "tag" | "team" | "type" | "gamemode" | "sort" => {
                if !parser.try_parse_string_or_identifier() {
                    parser.error("Expected string or identifier");
                    CSTEntitySelector::recover_option(parser);
                }
            }

            "scores" => {
                CSTEntitySelector::parse_scores_option(parser);
            }

            "distance" | "level" => {
                parser.parse_range();
            }

            _ => {
                parser.error_with_len_at(
                    name_position,
                    "Unknown entity selector option",
                    key_string.len(),
                );
                CSTEntitySelector::recover_option(parser);
            }
        }

        parser.finish_node();
        parser.finish_node();

        true
    }

    fn parse_scores_option(parser: &mut Parser) {
        if !parser.expect_char('{', "Expected '{'") {
            CSTEntitySelector::recover_option(parser);
            return;
        }

        parser.expect_char('}', "Expected '}'");
    }

    #[inline]
    fn recover_option(parser: &mut Parser) {
        parser.bump_until_char(&[',', ']']);
    }

    fn parse_options(parser: &mut Parser) {
        parser.start_node_bump(SyntaxKind::VariableEntitySelectorOptions, 1);
        parser.skip_whitespace();

        let mut is_first = true;

        loop {
            parser.skip_whitespace();

            if parser.is_eof() || parser.peek_char() == Some(']') {
                parser.bump_char();

                break;
            }

            if !is_first {
                if !parser.expect_char(',', "Expected ',' or ']'") {
                    Self::recover_option(parser);

                    continue;
                }

                parser.skip_whitespace();
            }

            if !Self::parse_option(parser) {
                Self::recover_option(parser);
            }

            is_first = false;
        }

        parser.finish_node();
    }

    #[must_use]
    pub fn cast(node: &'a CSTNodeType) -> Option<Self> {
        match node.kind()? {
            SyntaxKind::VariableEntitySelector => {
                CSTVariableEntitySelector::cast(node).map(CSTEntitySelector::Variable)
            }
            SyntaxKind::EntitySelectorName => Some(CSTEntitySelector::Name(node.span())),
            _ => {
                #[cfg(debug_assertions)]
                println!("Failed to cast node {:?} to CSTEntitySelector", node);

                None
            }
        }
    }

    #[must_use]
    pub fn lower(self, text: &str) -> Option<HighEntitySelector> {
        match self {
            CSTEntitySelector::Variable(selector) => selector.lower(text),
            CSTEntitySelector::Name(name_span) => Some(HighEntitySelector::Name(
                text[name_span.into_range()].to_string(),
            )),
        }
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        match self {
            CSTEntitySelector::Variable(selector) => {
                selector.collect_semantic_tokens(tokens);
            }
            CSTEntitySelector::Name(name_span) => {
                tokens.push(SemanticToken::new(*name_span, SemanticTokenType::Variable));
            }
        }
    }
}
