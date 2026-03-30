use kelp_core::high::{
    command::{Command, stopwatch::StopwatchCommand},
    expression::{Expression, ExpressionKind},
    semantic_analysis::SemanticAnalysisContext,
};

use crate::{
    cst::{
        CSTStopwatchCommandExpression, CSTStopwatchCommandExpressionCreate,
        CSTStopwatchCommandExpressionOptions, CSTStopwatchCommandExpressionQuery,
        CSTStopwatchCommandExpressionRemove, CSTStopwatchCommandExpressionRestart,
    },
    parser::Parser,
    resource_location::{lower_resource_location, try_parse_resource_location},
    span::span_of_cst_node,
    syntax::SyntaxKind,
};

fn try_parse_stopwatch_command_expression_create(parser: &mut Parser) {
    parser.start_node(SyntaxKind::StopwatchCommandExpressionCreate);

    parser.bump_identifier_kind(SyntaxKind::CreateKeyword, "create");

    parser.expect_inline_whitespace();

    if !try_parse_resource_location(parser) {
        parser.error("Expected resource location");
    }

    parser.finish_node();
}

fn try_parse_stopwatch_command_expression_query(parser: &mut Parser) {
    parser.start_node(SyntaxKind::StopwatchCommandExpressionQuery);

    parser.bump_identifier_kind(SyntaxKind::QueryKeyword, "query");

    parser.expect_inline_whitespace();

    if !try_parse_resource_location(parser) {
        parser.error("Expected resource location");
    }

    parser.skip_inline_whitespace();

    parser.try_parse_fractional_value();

    parser.finish_node();
}

fn try_parse_stopwatch_command_expression_remove(parser: &mut Parser) {
    parser.start_node(SyntaxKind::StopwatchCommandExpressionRemove);

    parser.bump_identifier_kind(SyntaxKind::RemoveKeyword, "remove");

    parser.expect_inline_whitespace();

    if !try_parse_resource_location(parser) {
        parser.error("Expected resource location");
    }

    parser.finish_node();
}

fn try_parse_stopwatch_command_expression_restart(parser: &mut Parser) {
    parser.start_node(SyntaxKind::StopwatchCommandExpressionRestart);

    parser.bump_identifier_kind(SyntaxKind::RestartKeyword, "restart");

    parser.expect_inline_whitespace();

    if !try_parse_resource_location(parser) {
        parser.error("Expected resource location");
    }

    parser.finish_node();
}

fn try_parse_stopwatch_command_expression_options(parser: &mut Parser) -> bool {
    let Some(option) = parser.peek_identifier() else {
        return false;
    };

    match option {
        "create" => try_parse_stopwatch_command_expression_create(parser),
        "query" => try_parse_stopwatch_command_expression_query(parser),
        "remove" => try_parse_stopwatch_command_expression_remove(parser),
        "restart" => try_parse_stopwatch_command_expression_restart(parser),
        _ => return false,
    }

    true
}

pub fn try_parse_stopwatch_command_expression(parser: &mut Parser) -> bool {
    let state = parser.save_state();

    parser.start_node(SyntaxKind::StopwatchCommandExpression);
    parser.bump_str(SyntaxKind::StopwatchKeyword, "stopwatch");

    if !parser.expect_inline_whitespace() || !try_parse_stopwatch_command_expression_options(parser)
    {
        parser.restore_state(state);

        return false;
    }

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
fn lower_stopwatch_command_expression_create(
    node: CSTStopwatchCommandExpressionCreate,
    ctx: &mut SemanticAnalysisContext,
) -> Option<StopwatchCommand> {
    let resource_location = lower_resource_location(node.resource_location()?, ctx)?;

    Some(StopwatchCommand::Create(resource_location))
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
fn lower_stopwatch_command_expression_query(
    node: CSTStopwatchCommandExpressionQuery,
    ctx: &mut SemanticAnalysisContext,
) -> Option<StopwatchCommand> {
    let resource_location = lower_resource_location(node.resource_location()?, ctx)?;

    let scale = node
        .fractional_value_token()
        .and_then(|token| token.text().parse().ok());

    Some(StopwatchCommand::Query(resource_location, scale))
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
fn lower_stopwatch_command_expression_remove(
    node: CSTStopwatchCommandExpressionRemove,
    ctx: &mut SemanticAnalysisContext,
) -> Option<StopwatchCommand> {
    let resource_location = lower_resource_location(node.resource_location()?, ctx)?;

    Some(StopwatchCommand::Remove(resource_location))
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
fn lower_stopwatch_command_expression_restart(
    node: CSTStopwatchCommandExpressionRestart,
    ctx: &mut SemanticAnalysisContext,
) -> Option<StopwatchCommand> {
    let resource_location = lower_resource_location(node.resource_location()?, ctx)?;

    Some(StopwatchCommand::Restart(resource_location))
}

#[must_use]
fn lower_stopwatch_command_expression_options(
    node: CSTStopwatchCommandExpressionOptions,
    ctx: &mut SemanticAnalysisContext,
) -> Option<StopwatchCommand> {
    match node {
        CSTStopwatchCommandExpressionOptions::StopwatchCommandExpressionCreate(node) => {
            lower_stopwatch_command_expression_create(node, ctx)
        }
        CSTStopwatchCommandExpressionOptions::StopwatchCommandExpressionQuery(node) => {
            lower_stopwatch_command_expression_query(node, ctx)
        }
        CSTStopwatchCommandExpressionOptions::StopwatchCommandExpressionRemove(node) => {
            lower_stopwatch_command_expression_remove(node, ctx)
        }
        CSTStopwatchCommandExpressionOptions::StopwatchCommandExpressionRestart(node) => {
            lower_stopwatch_command_expression_restart(node, ctx)
        }
    }
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_stopwatch_command_expression(
    node: CSTStopwatchCommandExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let command = lower_stopwatch_command_expression_options(
        node.stopwatch_command_expression_options()?,
        ctx,
    )?;

    Some(ExpressionKind::Command(Box::new(Command::Stopwatch(command))).with_span(span))
}
