use kelp_core::high::{expression::Expression, semantic_analysis_context::SemanticAnalysisContext};

use crate::{
    cst::CSTCommandExpression,
    expression::command::{
        function::{lower_function_command_expression, try_parse_function_command_expression},
        r#return::{lower_return_command_expression, try_parse_return_command_expression},
        stopwatch::{lower_stopwatch_command_expression, try_parse_stopwatch_command_expression},
        tellraw::{lower_tellraw_command_expression, try_parse_tellraw_command_expression},
    },
    parser::Parser,
    syntax::SyntaxKind,
};

pub mod function;
pub mod r#return;
pub mod stopwatch;
pub mod tellraw;

pub fn try_parse_command_expression(parser: &mut Parser, name: &str) -> bool {
    let result = match name {
        "tellraw" => try_parse_tellraw_command_expression(parser),
        "function" => try_parse_function_command_expression(parser),
        "return" => try_parse_return_command_expression(parser),
        "stopwatch" => try_parse_stopwatch_command_expression(parser),
        _ => return false,
    };

    if !result {
        parser.start_node(SyntaxKind::PathExpression);
        parser.bump_identifier("tellraw");
        parser.finish_node();
    }

    true
}

pub fn lower_command_expression(
    node: CSTCommandExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    match node {
        CSTCommandExpression::TellrawCommandExpression(node) => {
            lower_tellraw_command_expression(node, ctx)
        }
        CSTCommandExpression::ReturnCommandExpression(node) => {
            lower_return_command_expression(node, ctx)
        }
        CSTCommandExpression::FunctionCommandExpression(node) => {
            lower_function_command_expression(node, ctx)
        }
        CSTCommandExpression::StopwatchCommandExpression(node) => {
            lower_stopwatch_command_expression(node, ctx)
        }
    }
}
