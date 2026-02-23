use kelp_core::{
    expression::literal::{LiteralExpression, LiteralExpressionKind},
    statement::Statement,
};
use ordered_float::NotNan;

use crate::{
    cstlib::{CSTNodeType, token::CSTToken},
    lower::statement::CSTStatement,
    syntax::SyntaxKind,
};

pub mod coordinates;
pub mod cst;
pub mod data;
pub mod data_type;
pub mod entity_selector;
pub mod expression;
pub mod pattern;
pub mod resource_location;
pub mod statement;
pub mod struct_field;

pub struct Lowerer;

impl Lowerer {
    pub fn lower_root(node: &CSTNodeType) -> Vec<Statement> {
        node.children()
            .filter_map(CSTStatement::cast)
            .filter_map(CSTStatement::lower)
            .collect()
    }

    fn split_suffix(text: &str) -> (&str, Option<char>) {
        match text.chars().last() {
            Some(c) if c.is_ascii_alphabetic() => (&text[..text.len() - c.len_utf8()], Some(c)),
            _ => (text, None),
        }
    }

    fn lower_literal_from_token(
        CSTToken { kind, text, span }: &CSTToken,
    ) -> Option<LiteralExpression> {
        let kind = match kind {
            SyntaxKind::Integer => {
                let (number, suffix) = Self::split_suffix(text);

                match suffix {
                    Some('b' | 'B') => LiteralExpressionKind::Byte(number.parse::<i8>().ok()?),
                    Some('s' | 'S') => LiteralExpressionKind::Short(number.parse::<i16>().ok()?),
                    Some('l' | 'L') => LiteralExpressionKind::Long(number.parse::<i64>().ok()?),
                    _ => LiteralExpressionKind::Integer(number.parse::<i32>().ok()?),
                }
            }

            SyntaxKind::Float => {
                let (number, suffix) = Self::split_suffix(text);

                match suffix {
                    Some('d' | 'D') => {
                        LiteralExpressionKind::Double(number.parse::<NotNan<f64>>().ok()?)
                    }
                    _ => LiteralExpressionKind::Float(number.parse::<NotNan<f32>>().ok()?),
                }
            }

            SyntaxKind::String => {
                LiteralExpressionKind::String(text.trim_matches('"').to_string().into())
            }

            SyntaxKind::Char => {
                LiteralExpressionKind::String(text.trim_matches('\'').to_string().into())
            }

            _ => return None,
        };

        Some(LiteralExpression { span: *span, kind })
    }
}
