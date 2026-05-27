use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTExpressionWithoutBlock, extension_traits::LowerableAstNode, lower_context::LowerContext,
};

pub mod as_cast;
pub mod assignment;
pub mod binary;
pub mod boolean;
pub mod call;
pub mod character;
pub mod command;
pub mod compound;
pub mod coordinates;
pub mod data;
pub mod entity_selector;
pub mod field_access;
pub mod index;
pub mod list;
pub mod method_call;
pub mod numeric;
pub mod parenthesized;
pub mod path;
pub mod resource_location;
pub mod r#return;
pub mod score;
pub mod string;
pub mod r#struct;
pub mod to_cast;
pub mod tuple;
pub mod unary;
pub mod underscore;
pub mod unit;

impl LowerableAstNode for CSTExpressionWithoutBlock {
    type Lowered = ParsedExpression;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        match self {
            Self::UnaryExpression(node) => node.lower(ctx),
            Self::PathExpression(node) => node.lower(ctx),
            Self::UnderscoreExpression(node) => node.lower(ctx),
            Self::BooleanExpression(node) => node.lower(ctx),
            Self::NumericExpression(node) => node.lower(ctx),
            Self::CharacterExpression(node) => node.lower(ctx),
            Self::StringExpression(node) => node.lower(ctx),
            Self::AssignmentExpression(node) => node.lower(ctx),
            Self::BinaryExpression(node) => node.lower(ctx),
            Self::DataExpression(node) => node.lower(ctx),
            Self::ScoreExpression(node) => node.lower(ctx),
            Self::CommandExpression(node) => node.lower(ctx),
            Self::UnitExpression(node) => node.lower(ctx),
            Self::CompoundExpression(node) => {
                let (span, compound) = node.lower(ctx)?;

                Some(ParsedExpressionKind::Compound(compound).with_span(span))
            }
            Self::ListExpression(node) => node.lower(ctx),
            Self::TupleExpression(node) => node.lower(ctx),
            Self::CallExpression(node) => node.lower(ctx),
            Self::ParenthesizedExpression(node) => node.lower(ctx),
            Self::AsCastExpression(node) => node.lower(ctx),
            Self::ToCastExpression(node) => node.lower(ctx),
            Self::StructExpression(node) => node.lower(ctx),
            Self::IndexExpression(node) => node.lower(ctx),
            Self::MethodCallExpression(node) => node.lower(ctx),
            Self::FieldAccessExpression(node) => node.lower(ctx),
            Self::ReturnExpression(node) => node.lower(ctx),
            Self::ResourceLocationExpression(node) => node.lower(ctx),
            Self::EntitySelectorExpression(node) => node.lower(ctx),
            Self::CoordinatesExpression(node) => node.lower(ctx),
        }
    }
}
