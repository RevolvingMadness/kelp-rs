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
    #[must_use]
    pub const fn with_span(self, span: Span) -> LiteralExpression {
        LiteralExpression { span, kind: self }
    }

    #[must_use]
    pub const fn get_pattern_type(&self) -> PatternType {
        match self {
            Self::Boolean(_) => PatternType::Boolean,
            Self::Byte(_) => PatternType::Byte,
            Self::Short(_) => PatternType::Short,
            Self::Integer(_) => PatternType::Integer,
            Self::Long(_) => PatternType::Long,
            Self::Float(_) => PatternType::Float,
            Self::Double(_) => PatternType::Double,
            Self::String(_) => PatternType::String,
        }
    }

    #[must_use]
    pub const fn get_data_type(&self) -> DataTypeKind {
        match self {
            Self::Boolean(_) => DataTypeKind::Boolean,
            Self::Byte(_) => DataTypeKind::Byte,
            Self::Short(_) => DataTypeKind::Short,
            Self::Integer(_) => DataTypeKind::Integer,
            Self::Long(_) => DataTypeKind::Long,
            Self::Float(_) => DataTypeKind::Float,
            Self::Double(_) => DataTypeKind::Double,
            Self::String(_) => DataTypeKind::String,
        }
    }

    #[must_use]
    pub fn try_as_i32(&self, force: bool) -> Option<i32> {
        Some(match self {
            Self::Byte(v) => i32::from(*v),
            Self::Short(v) => i32::from(*v),
            Self::Integer(v) => *v,
            Self::Long(v) => *v as i32,
            Self::Float(v) if force => v.into_inner() as i32,
            Self::Double(v) if force => v.into_inner() as i32,
            Self::String(HighSNBTString {
                snbt_string: SNBTString(_, v),
                ..
            }) if force => v.len() as i32,
            _ => return None,
        })
    }

    #[must_use]
    pub fn invert(self) -> Self {
        match self {
            Self::Boolean(value) => Self::Boolean(!value),
            _ => unreachable!("Cannot invert expression {:?}", self),
        }
    }

    #[must_use]
    pub fn negate(self) -> Self {
        match self {
            Self::Byte(value) => Self::Byte(-value),
            Self::Short(value) => Self::Short(-value),
            Self::Integer(value) => Self::Integer(-value),
            Self::Long(value) => Self::Long(-value),
            Self::Float(value) => Self::Float(-value),
            Self::Double(value) => Self::Double(-value),
            _ => unreachable!("Cannot negate expression {:?}", self),
        }
    }

    #[must_use]
    pub fn compare(self, operator: ComparisonOperator, other: Self) -> Option<Self> {
        Some(match (self, other) {
            (Self::Byte(left), Self::Byte(right)) => Self::Boolean(match operator {
                ComparisonOperator::LessThan => left < right,
                ComparisonOperator::LessThanOrEqualTo => left <= right,
                ComparisonOperator::GreaterThan => left > right,
                ComparisonOperator::GreaterThanOrEqualTo => left >= right,
                ComparisonOperator::EqualTo => left == right,
                ComparisonOperator::NotEqualTo => left != right,
            }),
            (Self::Short(left), Self::Short(right)) => Self::Boolean(match operator {
                ComparisonOperator::LessThan => left < right,
                ComparisonOperator::LessThanOrEqualTo => left <= right,
                ComparisonOperator::GreaterThan => left > right,
                ComparisonOperator::GreaterThanOrEqualTo => left >= right,
                ComparisonOperator::EqualTo => left == right,
                ComparisonOperator::NotEqualTo => left != right,
            }),
            (Self::Integer(left), Self::Integer(right)) => Self::Boolean(match operator {
                ComparisonOperator::LessThan => left < right,
                ComparisonOperator::LessThanOrEqualTo => left <= right,
                ComparisonOperator::GreaterThan => left > right,
                ComparisonOperator::GreaterThanOrEqualTo => left >= right,
                ComparisonOperator::EqualTo => left == right,
                ComparisonOperator::NotEqualTo => left != right,
            }),
            (Self::Long(left), Self::Long(right)) => Self::Boolean(match operator {
                ComparisonOperator::LessThan => left < right,
                ComparisonOperator::LessThanOrEqualTo => left <= right,
                ComparisonOperator::GreaterThan => left > right,
                ComparisonOperator::GreaterThanOrEqualTo => left >= right,
                ComparisonOperator::EqualTo => left == right,
                ComparisonOperator::NotEqualTo => left != right,
            }),
            (Self::Float(left), Self::Float(right)) => Self::Boolean(match operator {
                ComparisonOperator::LessThan => left < right,
                ComparisonOperator::LessThanOrEqualTo => left <= right,
                ComparisonOperator::GreaterThan => left > right,
                ComparisonOperator::GreaterThanOrEqualTo => left >= right,
                ComparisonOperator::EqualTo => left == right,
                ComparisonOperator::NotEqualTo => left != right,
            }),
            (Self::Double(left), Self::Double(right)) => Self::Boolean(match operator {
                ComparisonOperator::LessThan => left < right,
                ComparisonOperator::LessThanOrEqualTo => left <= right,
                ComparisonOperator::GreaterThan => left > right,
                ComparisonOperator::GreaterThanOrEqualTo => left >= right,
                ComparisonOperator::EqualTo => left == right,
                ComparisonOperator::NotEqualTo => left != right,
            }),
            _ => return None,
        })
    }

    pub fn to_execute_condition(
        &self,
        datapack: &mut HighDatapack,
    ) -> Option<(bool, ExecuteIfSubcommand)> {
        Some(match self {
            Self::Boolean(value) => (
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
            Self::Boolean(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::Byte(i8::from(*value))
                }
            }
            Self::Byte(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::Byte(*value)
                }
            }
            Self::Short(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::Short(*value)
                }
            }
            Self::Integer(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::Integer(*value)
                }
            }
            Self::Long(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::Long(*value)
                }
            }
            Self::Float(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::Float(*value)
                }
            }
            Self::Double(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::Double(*value)
                }
            }
            Self::String(string) => {
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
            Self::Boolean(value) => SNBT::Byte(i8::from(value)),
            Self::Byte(value) => SNBT::Byte(value),
            Self::Short(value) => SNBT::Short(value),
            Self::Integer(value) => SNBT::Integer(value),
            Self::Long(value) => SNBT::Long(value),
            Self::Float(value) => SNBT::Float(value),
            Self::Double(value) => SNBT::Double(value),
            Self::String(snbt_string) => SNBT::String(snbt_string.snbt_string),
        }
    }

    #[must_use]
    pub fn cast_to(self, data_type: DataTypeKind) -> Self {
        match (self, data_type) {
            (Self::Byte(value), DataTypeKind::Short) => Self::Short(i16::from(value)),
            (Self::Byte(value), DataTypeKind::Integer) => Self::Integer(i32::from(value)),
            (Self::Byte(value), DataTypeKind::Long) => Self::Long(i64::from(value)),
            (Self::Byte(value), DataTypeKind::Float) => {
                Self::Float(NotNan::new(f32::from(value)).unwrap())
            }
            (Self::Byte(value), DataTypeKind::Double) => {
                Self::Double(NotNan::new(f64::from(value)).unwrap())
            }

            (Self::Short(value), DataTypeKind::Byte) => Self::Byte(value as i8),
            (Self::Short(value), DataTypeKind::Integer) => Self::Integer(i32::from(value)),
            (Self::Short(value), DataTypeKind::Long) => Self::Long(i64::from(value)),
            (Self::Short(value), DataTypeKind::Float) => {
                Self::Float(NotNan::new(f32::from(value)).unwrap())
            }
            (Self::Short(value), DataTypeKind::Double) => {
                Self::Double(NotNan::new(f64::from(value)).unwrap())
            }

            (Self::Integer(value), DataTypeKind::Byte) => Self::Byte(value as i8),
            (Self::Integer(value), DataTypeKind::Short) => Self::Short(value as i16),
            (Self::Integer(value), DataTypeKind::Long) => Self::Long(i64::from(value)),
            (Self::Integer(value), DataTypeKind::Float) => {
                Self::Float(NotNan::new(value as f32).unwrap())
            }
            (Self::Integer(value), DataTypeKind::Double) => {
                Self::Double(NotNan::new(f64::from(value)).unwrap())
            }

            (Self::Long(value), DataTypeKind::Byte) => Self::Byte(value as i8),
            (Self::Long(value), DataTypeKind::Short) => Self::Short(value as i16),
            (Self::Long(value), DataTypeKind::Integer) => Self::Integer(value as i32),
            (Self::Long(value), DataTypeKind::Float) => {
                Self::Float(NotNan::new(value as f32).unwrap())
            }
            (Self::Long(value), DataTypeKind::Double) => {
                Self::Double(NotNan::new(value as f64).unwrap())
            }

            (Self::Float(value), DataTypeKind::Byte) => Self::Byte(value.into_inner() as i8),
            (Self::Float(value), DataTypeKind::Short) => Self::Short(value.into_inner() as i16),
            (Self::Float(value), DataTypeKind::Integer) => Self::Integer(value.into_inner() as i32),
            (Self::Float(value), DataTypeKind::Long) => Self::Long(value.into_inner() as i64),
            (Self::Float(value), DataTypeKind::Double) => Self::Double(value.into()),

            (Self::Double(value), DataTypeKind::Byte) => Self::Byte(value.into_inner() as i8),
            (Self::Double(value), DataTypeKind::Short) => Self::Short(value.into_inner() as i16),
            (Self::Double(value), DataTypeKind::Integer) => {
                Self::Integer(value.into_inner() as i32)
            }
            (Self::Double(value), DataTypeKind::Long) => Self::Long(value.into_inner() as i64),
            (Self::Double(value), DataTypeKind::Float) => {
                Self::Float(unsafe { NotNan::new_unchecked(value.into_inner() as f32) })
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
            Self::String(high_snbtstring) => high_snbtstring.perform_semantic_analysis(ctx, is_lhs),
            Self::Boolean(_)
            | Self::Byte(_)
            | Self::Short(_)
            | Self::Integer(_)
            | Self::Long(_)
            | Self::Float(_)
            | Self::Double(_) => Some(()),
        }
    }

    #[must_use]
    pub fn perform_arithmetic(&self, operator: ArithmeticOperator, right: Self) -> Self {
        match (self, right) {
            (Self::Byte(left), Self::Byte(right)) => Self::Byte(match operator {
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
            }),

            (Self::Short(left), Self::Short(right)) => Self::Short(match operator {
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
            }),

            (Self::Integer(left), Self::Integer(right)) => Self::Integer(match operator {
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
            }),

            (Self::Long(left), Self::Long(right)) => Self::Long(match operator {
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
            }),

            (Self::Float(left), Self::Float(right)) => Self::Float(match operator {
                ArithmeticOperator::Add => left + right,
                ArithmeticOperator::Subtract => left - right,
                ArithmeticOperator::Multiply => left * right,
                ArithmeticOperator::FloorDivide => left / right,
                ArithmeticOperator::Modulo => left % right,
                _ => unreachable!(),
            }),

            (Self::Double(left), Self::Double(right)) => Self::Double(match operator {
                ArithmeticOperator::Add => left + right,
                ArithmeticOperator::Subtract => left - right,
                ArithmeticOperator::Multiply => left * right,
                ArithmeticOperator::FloorDivide => left / right,
                ArithmeticOperator::Modulo => left % right,
                _ => unreachable!(),
            }),

            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct LiteralExpression {
    pub span: Span,
    pub kind: LiteralExpressionKind,
}
