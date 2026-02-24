use minecraft_command_types::{
    command::{
        PlayerScore,
        execute::{ExecuteIfSubcommand, ScoreComparison},
        scoreboard::PlayersScoreboardCommand,
    },
    range::IntegerRange,
    snbt::{SNBT, SNBTString},
};
use minecraft_command_types_derive::HasMacro;
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext,
    data_type::DataTypeKind,
    datapack::HighDatapack,
    expression::utils::push_scoreboard_players,
    high::snbt_string::HighSNBTString,
    operator::{ArithmeticOperator, ComparisonOperator},
    pattern_type::PatternType,
    semantic_analysis_context::SemanticAnalysisContext,
    span::Span,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum LiteralExpressionKind {
    Boolean(bool),
    Byte(i8),
    Short(i16),
    Integer(i32),
    Long(i64),
    Float(NotNan<f32>),
    Double(NotNan<f64>),
    String(HighSNBTString),
}

impl LiteralExpressionKind {
    #[inline]
    #[must_use]
    pub fn with_span(self, span: Span) -> LiteralExpression {
        LiteralExpression { span, kind: self }
    }

    #[must_use]
    pub fn get_pattern_type(&self) -> PatternType {
        match self {
            LiteralExpressionKind::Boolean(_) => PatternType::Boolean,
            LiteralExpressionKind::Byte(_) => PatternType::Byte,
            LiteralExpressionKind::Short(_) => PatternType::Short,
            LiteralExpressionKind::Integer(_) => PatternType::Integer,
            LiteralExpressionKind::Long(_) => PatternType::Long,
            LiteralExpressionKind::Float(_) => PatternType::Float,
            LiteralExpressionKind::Double(_) => PatternType::Double,
            LiteralExpressionKind::String(_) => PatternType::String,
        }
    }

    #[must_use]
    pub fn get_data_type(&self) -> DataTypeKind {
        match self {
            LiteralExpressionKind::Boolean(_) => DataTypeKind::Boolean,
            LiteralExpressionKind::Byte(_) => DataTypeKind::Byte,
            LiteralExpressionKind::Short(_) => DataTypeKind::Short,
            LiteralExpressionKind::Integer(_) => DataTypeKind::Integer,
            LiteralExpressionKind::Long(_) => DataTypeKind::Long,
            LiteralExpressionKind::Float(_) => DataTypeKind::Float,
            LiteralExpressionKind::Double(_) => DataTypeKind::Double,
            LiteralExpressionKind::String(_) => DataTypeKind::String,
        }
    }

    #[must_use]
    pub fn try_as_i32(&self, force: bool) -> Option<i32> {
        Some(match self {
            LiteralExpressionKind::Byte(v) => i32::from(*v),
            LiteralExpressionKind::Short(v) => i32::from(*v),
            LiteralExpressionKind::Integer(v) => *v,
            LiteralExpressionKind::Long(v) => *v as i32,
            LiteralExpressionKind::Float(v) if force => v.into_inner() as i32,
            LiteralExpressionKind::Double(v) if force => v.into_inner() as i32,
            LiteralExpressionKind::String(HighSNBTString {
                snbt_string: SNBTString(_, v),
                ..
            }) if force => v.len() as i32,
            _ => return None,
        })
    }

    #[must_use]
    pub fn invert(self) -> LiteralExpressionKind {
        match self {
            LiteralExpressionKind::Boolean(value) => LiteralExpressionKind::Boolean(!value),
            _ => unreachable!("Cannot invert expression {:?}", self),
        }
    }

    #[must_use]
    pub fn negate(self) -> LiteralExpressionKind {
        match self {
            LiteralExpressionKind::Byte(value) => LiteralExpressionKind::Byte(-value),
            LiteralExpressionKind::Short(value) => LiteralExpressionKind::Short(-value),
            LiteralExpressionKind::Integer(value) => LiteralExpressionKind::Integer(-value),
            LiteralExpressionKind::Long(value) => LiteralExpressionKind::Long(-value),
            LiteralExpressionKind::Float(value) => LiteralExpressionKind::Float(-value),
            LiteralExpressionKind::Double(value) => LiteralExpressionKind::Double(-value),
            _ => unreachable!("Cannot negate expression {:?}", self),
        }
    }

    #[must_use]
    pub fn compare(
        self,
        operator: ComparisonOperator,
        other: LiteralExpressionKind,
    ) -> Option<LiteralExpressionKind> {
        Some(match (self, other) {
            (LiteralExpressionKind::Byte(left), LiteralExpressionKind::Byte(right)) => {
                LiteralExpressionKind::Boolean(match operator {
                    ComparisonOperator::LessThan => left < right,
                    ComparisonOperator::LessThanOrEqualTo => left <= right,
                    ComparisonOperator::GreaterThan => left > right,
                    ComparisonOperator::GreaterThanOrEqualTo => left >= right,
                    ComparisonOperator::EqualTo => left == right,
                    ComparisonOperator::NotEqualTo => left != right,
                })
            }
            (LiteralExpressionKind::Short(left), LiteralExpressionKind::Short(right)) => {
                LiteralExpressionKind::Boolean(match operator {
                    ComparisonOperator::LessThan => left < right,
                    ComparisonOperator::LessThanOrEqualTo => left <= right,
                    ComparisonOperator::GreaterThan => left > right,
                    ComparisonOperator::GreaterThanOrEqualTo => left >= right,
                    ComparisonOperator::EqualTo => left == right,
                    ComparisonOperator::NotEqualTo => left != right,
                })
            }
            (LiteralExpressionKind::Integer(left), LiteralExpressionKind::Integer(right)) => {
                LiteralExpressionKind::Boolean(match operator {
                    ComparisonOperator::LessThan => left < right,
                    ComparisonOperator::LessThanOrEqualTo => left <= right,
                    ComparisonOperator::GreaterThan => left > right,
                    ComparisonOperator::GreaterThanOrEqualTo => left >= right,
                    ComparisonOperator::EqualTo => left == right,
                    ComparisonOperator::NotEqualTo => left != right,
                })
            }
            (LiteralExpressionKind::Long(left), LiteralExpressionKind::Long(right)) => {
                LiteralExpressionKind::Boolean(match operator {
                    ComparisonOperator::LessThan => left < right,
                    ComparisonOperator::LessThanOrEqualTo => left <= right,
                    ComparisonOperator::GreaterThan => left > right,
                    ComparisonOperator::GreaterThanOrEqualTo => left >= right,
                    ComparisonOperator::EqualTo => left == right,
                    ComparisonOperator::NotEqualTo => left != right,
                })
            }
            (LiteralExpressionKind::Float(left), LiteralExpressionKind::Float(right)) => {
                LiteralExpressionKind::Boolean(match operator {
                    ComparisonOperator::LessThan => left < right,
                    ComparisonOperator::LessThanOrEqualTo => left <= right,
                    ComparisonOperator::GreaterThan => left > right,
                    ComparisonOperator::GreaterThanOrEqualTo => left >= right,
                    ComparisonOperator::EqualTo => left == right,
                    ComparisonOperator::NotEqualTo => left != right,
                })
            }
            (LiteralExpressionKind::Double(left), LiteralExpressionKind::Double(right)) => {
                LiteralExpressionKind::Boolean(match operator {
                    ComparisonOperator::LessThan => left < right,
                    ComparisonOperator::LessThanOrEqualTo => left <= right,
                    ComparisonOperator::GreaterThan => left > right,
                    ComparisonOperator::GreaterThanOrEqualTo => left >= right,
                    ComparisonOperator::EqualTo => left == right,
                    ComparisonOperator::NotEqualTo => left != right,
                })
            }
            _ => return None,
        })
    }

    pub fn to_execute_condition(
        &self,
        datapack: &mut HighDatapack,
    ) -> Option<(bool, ExecuteIfSubcommand)> {
        Some(match self {
            LiteralExpressionKind::Boolean(value) => (
                false,
                ExecuteIfSubcommand::Score(
                    datapack.get_constant_score(1).score,
                    ScoreComparison::Range(IntegerRange::new_single(i32::from(*value))),
                    None,
                ),
            ),
            _ => return None,
        })
    }

    #[must_use]
    pub fn as_text_component(&self, force_display: bool) -> SNBT {
        match self {
            LiteralExpressionKind::Boolean(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::Byte(i8::from(*value))
                }
            }
            LiteralExpressionKind::Byte(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::Byte(*value)
                }
            }
            LiteralExpressionKind::Short(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::Short(*value)
                }
            }
            LiteralExpressionKind::Integer(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::Integer(*value)
                }
            }
            LiteralExpressionKind::Long(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::Long(*value)
                }
            }
            LiteralExpressionKind::Float(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::Float(*value)
                }
            }
            LiteralExpressionKind::Double(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::Double(*value)
                }
            }
            LiteralExpressionKind::String(string) => {
                if force_display {
                    SNBT::string(format!("\"{}\"", string.snbt_string.1))
                } else {
                    SNBT::String(string.snbt_string.clone())
                }
            }
        }
    }

    pub fn assign_to_score(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: PlayerScore,
    ) {
        match self {
            Self::Boolean(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, i32::from(value)),
                );
            }
            Self::Byte(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, i32::from(value)),
                );
            }
            Self::Short(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, i32::from(value)),
                );
            }
            Self::Integer(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, value),
                );
            }
            Self::Long(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, value as i32),
                );
            }
            Self::Float(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, value.into_inner() as i32),
                );
            }
            Self::Double(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, value.into_inner() as i32),
                );
            }
            Self::String(HighSNBTString {
                snbt_string: SNBTString(_, value),
                ..
            }) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, value.len() as i32),
                );
            }
        }
    }

    #[must_use]
    pub fn into_snbt(self) -> SNBT {
        match self {
            LiteralExpressionKind::Boolean(value) => SNBT::Byte(i8::from(value)),
            LiteralExpressionKind::Byte(value) => SNBT::Byte(value),
            LiteralExpressionKind::Short(value) => SNBT::Short(value),
            LiteralExpressionKind::Integer(value) => SNBT::Integer(value),
            LiteralExpressionKind::Long(value) => SNBT::Long(value),
            LiteralExpressionKind::Float(value) => SNBT::Float(value),
            LiteralExpressionKind::Double(value) => SNBT::Double(value),
            LiteralExpressionKind::String(snbt_string) => SNBT::String(snbt_string.snbt_string),
        }
    }

    #[must_use]
    pub fn cast_to(self, data_type: DataTypeKind) -> LiteralExpressionKind {
        match (self, data_type) {
            (LiteralExpressionKind::Byte(value), DataTypeKind::Short) => {
                LiteralExpressionKind::Short(i16::from(value))
            }
            (LiteralExpressionKind::Byte(value), DataTypeKind::Integer) => {
                LiteralExpressionKind::Integer(i32::from(value))
            }
            (LiteralExpressionKind::Byte(value), DataTypeKind::Long) => {
                LiteralExpressionKind::Long(i64::from(value))
            }
            (LiteralExpressionKind::Byte(value), DataTypeKind::Float) => {
                LiteralExpressionKind::Float(NotNan::new(f32::from(value)).unwrap())
            }
            (LiteralExpressionKind::Byte(value), DataTypeKind::Double) => {
                LiteralExpressionKind::Double(NotNan::new(f64::from(value)).unwrap())
            }

            (LiteralExpressionKind::Short(value), DataTypeKind::Byte) => {
                LiteralExpressionKind::Byte(value as i8)
            }
            (LiteralExpressionKind::Short(value), DataTypeKind::Integer) => {
                LiteralExpressionKind::Integer(i32::from(value))
            }
            (LiteralExpressionKind::Short(value), DataTypeKind::Long) => {
                LiteralExpressionKind::Long(i64::from(value))
            }
            (LiteralExpressionKind::Short(value), DataTypeKind::Float) => {
                LiteralExpressionKind::Float(NotNan::new(f32::from(value)).unwrap())
            }
            (LiteralExpressionKind::Short(value), DataTypeKind::Double) => {
                LiteralExpressionKind::Double(NotNan::new(f64::from(value)).unwrap())
            }

            (LiteralExpressionKind::Integer(value), DataTypeKind::Byte) => {
                LiteralExpressionKind::Byte(value as i8)
            }
            (LiteralExpressionKind::Integer(value), DataTypeKind::Short) => {
                LiteralExpressionKind::Short(value as i16)
            }
            (LiteralExpressionKind::Integer(value), DataTypeKind::Long) => {
                LiteralExpressionKind::Long(i64::from(value))
            }
            (LiteralExpressionKind::Integer(value), DataTypeKind::Float) => {
                LiteralExpressionKind::Float(NotNan::new(value as f32).unwrap())
            }
            (LiteralExpressionKind::Integer(value), DataTypeKind::Double) => {
                LiteralExpressionKind::Double(NotNan::new(f64::from(value)).unwrap())
            }

            (LiteralExpressionKind::Long(value), DataTypeKind::Byte) => {
                LiteralExpressionKind::Byte(value as i8)
            }
            (LiteralExpressionKind::Long(value), DataTypeKind::Short) => {
                LiteralExpressionKind::Short(value as i16)
            }
            (LiteralExpressionKind::Long(value), DataTypeKind::Integer) => {
                LiteralExpressionKind::Integer(value as i32)
            }
            (LiteralExpressionKind::Long(value), DataTypeKind::Float) => {
                LiteralExpressionKind::Float(NotNan::new(value as f32).unwrap())
            }
            (LiteralExpressionKind::Long(value), DataTypeKind::Double) => {
                LiteralExpressionKind::Double(NotNan::new(value as f64).unwrap())
            }

            (LiteralExpressionKind::Float(value), DataTypeKind::Byte) => {
                LiteralExpressionKind::Byte(value.into_inner() as i8)
            }
            (LiteralExpressionKind::Float(value), DataTypeKind::Short) => {
                LiteralExpressionKind::Short(value.into_inner() as i16)
            }
            (LiteralExpressionKind::Float(value), DataTypeKind::Integer) => {
                LiteralExpressionKind::Integer(value.into_inner() as i32)
            }
            (LiteralExpressionKind::Float(value), DataTypeKind::Long) => {
                LiteralExpressionKind::Long(value.into_inner() as i64)
            }
            (LiteralExpressionKind::Float(value), DataTypeKind::Double) => {
                LiteralExpressionKind::Double(value.into())
            }

            (LiteralExpressionKind::Double(value), DataTypeKind::Byte) => {
                LiteralExpressionKind::Byte(value.into_inner() as i8)
            }
            (LiteralExpressionKind::Double(value), DataTypeKind::Short) => {
                LiteralExpressionKind::Short(value.into_inner() as i16)
            }
            (LiteralExpressionKind::Double(value), DataTypeKind::Integer) => {
                LiteralExpressionKind::Integer(value.into_inner() as i32)
            }
            (LiteralExpressionKind::Double(value), DataTypeKind::Long) => {
                LiteralExpressionKind::Long(value.into_inner() as i64)
            }
            (LiteralExpressionKind::Double(value), DataTypeKind::Float) => {
                LiteralExpressionKind::Float(unsafe {
                    NotNan::new_unchecked(value.into_inner() as f32)
                })
            }
            (self_, data_type) => {
                unreachable!("Cannot cast expression {:?} to type {}", self_, data_type)
            }
        }
    }

    #[must_use]
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            LiteralExpressionKind::String(high_snbtstring) => {
                high_snbtstring.perform_semantic_analysis(ctx, is_lhs)
            }
            LiteralExpressionKind::Boolean(_)
            | LiteralExpressionKind::Byte(_)
            | LiteralExpressionKind::Short(_)
            | LiteralExpressionKind::Integer(_)
            | LiteralExpressionKind::Long(_)
            | LiteralExpressionKind::Float(_)
            | LiteralExpressionKind::Double(_) => Some(()),
        }
    }

    #[must_use]
    pub fn perform_arithmetic(
        &self,
        operator: ArithmeticOperator,
        right: LiteralExpressionKind,
    ) -> LiteralExpressionKind {
        match (self, right) {
            (LiteralExpressionKind::Byte(left), LiteralExpressionKind::Byte(right)) => {
                LiteralExpressionKind::Byte(match operator {
                    ArithmeticOperator::Add => left.wrapping_add(right),
                    ArithmeticOperator::Subtract => left.wrapping_sub(right),
                    ArithmeticOperator::Multiply => left.wrapping_mul(right),
                    ArithmeticOperator::FloorDivide => left.wrapping_div(right),
                    ArithmeticOperator::Modulo => left % right,
                    ArithmeticOperator::And => left & right,
                    ArithmeticOperator::Or => left | right,
                    ArithmeticOperator::LeftShift => left << right,
                    ArithmeticOperator::RightShift => left >> right,
                    ArithmeticOperator::Swap => unreachable!(),
                })
            }

            (LiteralExpressionKind::Short(left), LiteralExpressionKind::Short(right)) => {
                LiteralExpressionKind::Short(match operator {
                    ArithmeticOperator::Add => left.wrapping_add(right),
                    ArithmeticOperator::Subtract => left.wrapping_sub(right),
                    ArithmeticOperator::Multiply => left.wrapping_mul(right),
                    ArithmeticOperator::FloorDivide => left.wrapping_div(right),
                    ArithmeticOperator::Modulo => left % right,
                    ArithmeticOperator::And => left & right,
                    ArithmeticOperator::Or => left | right,
                    ArithmeticOperator::LeftShift => left << right,
                    ArithmeticOperator::RightShift => left >> right,
                    ArithmeticOperator::Swap => unreachable!(),
                })
            }

            (LiteralExpressionKind::Integer(left), LiteralExpressionKind::Integer(right)) => {
                LiteralExpressionKind::Integer(match operator {
                    ArithmeticOperator::Add => left.wrapping_add(right),
                    ArithmeticOperator::Subtract => left.wrapping_sub(right),
                    ArithmeticOperator::Multiply => left.wrapping_mul(right),
                    ArithmeticOperator::FloorDivide => left.wrapping_div(right),
                    ArithmeticOperator::Modulo => left % right,
                    ArithmeticOperator::And => left & right,
                    ArithmeticOperator::Or => left | right,
                    ArithmeticOperator::LeftShift => left << right,
                    ArithmeticOperator::RightShift => left >> right,
                    ArithmeticOperator::Swap => unreachable!(),
                })
            }

            (LiteralExpressionKind::Long(left), LiteralExpressionKind::Long(right)) => {
                LiteralExpressionKind::Long(match operator {
                    ArithmeticOperator::Add => left.wrapping_add(right),
                    ArithmeticOperator::Subtract => left.wrapping_sub(right),
                    ArithmeticOperator::Multiply => left.wrapping_mul(right),
                    ArithmeticOperator::FloorDivide => left.wrapping_div(right),
                    ArithmeticOperator::Modulo => left % right,
                    ArithmeticOperator::And => left & right,
                    ArithmeticOperator::Or => left | right,
                    ArithmeticOperator::LeftShift => left << right,
                    ArithmeticOperator::RightShift => left >> right,
                    ArithmeticOperator::Swap => unreachable!(),
                })
            }

            (LiteralExpressionKind::Float(left), LiteralExpressionKind::Float(right)) => {
                LiteralExpressionKind::Float(match operator {
                    ArithmeticOperator::Add => left + right,
                    ArithmeticOperator::Subtract => left - right,
                    ArithmeticOperator::Multiply => left * right,
                    ArithmeticOperator::FloorDivide => left / right,
                    ArithmeticOperator::Modulo => left % right,
                    _ => unreachable!(),
                })
            }

            (LiteralExpressionKind::Double(left), LiteralExpressionKind::Double(right)) => {
                LiteralExpressionKind::Double(match operator {
                    ArithmeticOperator::Add => left + right,
                    ArithmeticOperator::Subtract => left - right,
                    ArithmeticOperator::Multiply => left * right,
                    ArithmeticOperator::FloorDivide => left / right,
                    ArithmeticOperator::Modulo => left % right,
                    _ => unreachable!(),
                })
            }

            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct LiteralExpression {
    pub span: Span,
    pub kind: LiteralExpressionKind,
}
