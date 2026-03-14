use kelp_core::high::entity_selector::EntitySelector;
use minecraft_command_types::entity_selector::EntitySelectorVariable;

use crate::{
    cst::{CSTEntitySelector, CSTNameEntitySelector, CSTVariableEntitySelector},
    parser::Parser,
    syntax::SyntaxKind,
};

pub fn try_parse_entity_selector(parser: &mut Parser) -> bool {
    if parser.try_start_node_bump('@', SyntaxKind::VariableEntitySelector) {
        if let Some(text) = parser.peek_identifier() {
            parser.add_token(SyntaxKind::EntitySelectorVariable, text.len());
        } else {
            parser.error("Expected entity selector variable");
        }

        if parser.peek_char() == Some('[') {
            parse_options(parser);
        }

        parser.finish_node();
    } else if let Some(name) = parser.peek_identifier() {
        parser.start_node(SyntaxKind::NameEntitySelector);
        parser.add_token(SyntaxKind::Identifier, name.len());
        parser.finish_node();
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
        recover_option(parser);
        parser.finish_node();

        return false;
    };

    let key_string = key_name.to_string();
    let name_position = parser.pos;
    parser.bump_identifier(key_name);
    parser.skip_whitespace();

    if !parser.expect_char('=', "Expected '=' after option name") {
        recover_option(parser);
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
                recover_option(parser);
            }
        }

        "scores" => {
            parse_scores_option(parser);
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
            recover_option(parser);
        }
    }

    parser.finish_node();
    parser.finish_node();

    true
}

fn parse_scores_option(parser: &mut Parser) {
    if !parser.expect_char('{', "Expected '{'") {
        recover_option(parser);
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
                recover_option(parser);

                continue;
            }

            parser.skip_whitespace();
        }

        if !parse_option(parser) {
            recover_option(parser);
        }

        is_first = false;
    }

    parser.finish_node();
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_variable_entity_selector(
    node: CSTVariableEntitySelector,
) -> Option<EntitySelector> {
    let variable_token = node.entity_selector_variable_token()?;
    let variable = variable_token.text();

    let variable = match variable {
        "a" => EntitySelectorVariable::A,
        "e" => EntitySelectorVariable::E,
        "n" => EntitySelectorVariable::N,
        "p" => EntitySelectorVariable::P,
        "r" => EntitySelectorVariable::R,
        "s" => EntitySelectorVariable::S,
        _ => return None,
    };

    Some(EntitySelector::Variable(variable, Vec::new()))
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_name_entity_selector(node: CSTNameEntitySelector) -> Option<EntitySelector> {
    let name_token = node.identifier_token()?;
    let name = name_token.text();

    Some(EntitySelector::Name(name.to_string()))
}

#[must_use]
pub fn lower_entity_selector(node: CSTEntitySelector) -> Option<EntitySelector> {
    match node {
        CSTEntitySelector::VariableEntitySelector(node) => lower_variable_entity_selector(node),
        CSTEntitySelector::NameEntitySelector(node) => lower_name_entity_selector(node),
    }
}
