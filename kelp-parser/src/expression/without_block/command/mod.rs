use kelp_core::parsed::expression::ParsedExpression;

use crate::{
    cst::{
        CSTCommandExpression, CSTFunctionCommandExpression, CSTStopwatchCommandExpression,
        CSTTellrawCommandExpression,
    },
    extension_traits::{LowerableAstNode, ParsableAstNode},
    lower_context::LowerContext,
    parser::Parser,
};

pub mod function;
pub mod stopwatch;
pub mod tellraw;

impl ParsableAstNode for CSTCommandExpression {
    fn try_parse(parser: &mut Parser) -> bool {
        let Some(name) = parser.peek_identifier() else {
            return false;
        };

        match name {
            "tellraw" => CSTTellrawCommandExpression::try_parse(parser),
            "function" => CSTFunctionCommandExpression::try_parse(parser),
            "stopwatch" => CSTStopwatchCommandExpression::try_parse(parser),
            _ => false,
        }
    }
}

impl LowerableAstNode for CSTCommandExpression {
    type Lowered = ParsedExpression;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        match self {
            Self::TellrawCommandExpression(node) => node.lower(ctx),
            Self::FunctionCommandExpression(node) => node.lower(ctx),
            Self::StopwatchCommandExpression(node) => node.lower(ctx),
        }
    }
}
