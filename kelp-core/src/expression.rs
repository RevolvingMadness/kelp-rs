use std::{collections::BTreeMap, fmt::Display};

use minecraft_command_types::{
    command::{
        Command, PlayerScore,
        data::{DataCommand, DataCommandModification, DataCommandModificationMode, DataTarget},
        enums::{score_operation_operator::ScoreOperationOperator, store_type::StoreType},
        execute::{
            ExecuteIfSubcommand, ExecuteStoreSubcommand, ExecuteSubcommand, ScoreComparison,
            ScoreComparisonOperator,
        },
        r#return::ReturnCommand,
        scoreboard::{PlayersScoreboardCommand, ScoreboardCommand},
    },
    has_macro::HasMacro,
    impl_has_macro_false,
    nbt_path::{NbtPath, NbtPathNode, SNBTCompound},
    range::IntegerRange,
    resource_location::ResourceLocation,
    snbt::{SNBT, SNBTString},
};
use minecraft_command_types_derive::HasMacro;
use ordered_float::NotNan;
use parser_rs::parser_range::ParserRange;

use crate::{
    command::PlayerScoreExt,
    compile_context::CompileContext,
    data_type::{DataTypeKind, high::HighDataType},
    datapack::HighDatapack,
    high::{
        command::{HighCommand, execute::subcommand::r#if::HighExecuteIfSubcommand},
        data::HighDataTarget,
        nbt_path::HighNbtPath,
        player_score::HighPlayerScore,
    },
    pattern_type::PatternType,
    place::{Place, PlaceType},
    runtime_storage_type::RuntimeStorageType,
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
    trait_ext::OptionIterExt,
};

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum ArithmeticOperator {
    Add,
    Subtract,
    Multiply,
    FloorDivide,
    Modulo,
    And,
    Or,
    LeftShift,
    RightShift,
    Swap,
}

impl Display for ArithmeticOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            ArithmeticOperator::Add => "+",
            ArithmeticOperator::Subtract => "-",
            ArithmeticOperator::Multiply => "*",
            ArithmeticOperator::FloorDivide => "/",
            ArithmeticOperator::Modulo => "%",
            ArithmeticOperator::And => "&",
            ArithmeticOperator::Or => "|",
            ArithmeticOperator::LeftShift => "<<",
            ArithmeticOperator::RightShift => ">>",
            ArithmeticOperator::Swap => "><",
        })
    }
}

impl ArithmeticOperator {
    pub fn is_additive(self) -> bool {
        matches!(self, ArithmeticOperator::Add | ArithmeticOperator::Subtract)
    }

    pub fn into_scoreboard_players_operation_operator(self) -> Option<ScoreOperationOperator> {
        match self {
            ArithmeticOperator::Add => Some(ScoreOperationOperator::Add),
            ArithmeticOperator::Subtract => Some(ScoreOperationOperator::Subtract),
            ArithmeticOperator::Multiply => Some(ScoreOperationOperator::Multiply),
            ArithmeticOperator::FloorDivide => Some(ScoreOperationOperator::Divide),
            ArithmeticOperator::Modulo => Some(ScoreOperationOperator::Modulo),
            ArithmeticOperator::Swap => Some(ScoreOperationOperator::Swap),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum LogicalOperator {
    And,
    Or,
}

impl Display for LogicalOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            LogicalOperator::And => "&&",
            LogicalOperator::Or => "||",
        })
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum ComparisonOperator {
    LessThan,
    LessThanOrEqualTo,
    GreaterThan,
    GreaterThanOrEqualTo,
    EqualTo,
    NotEqualTo,
}

impl Display for ComparisonOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            ComparisonOperator::LessThan => "<",
            ComparisonOperator::LessThanOrEqualTo => "<=",
            ComparisonOperator::GreaterThan => ">",
            ComparisonOperator::GreaterThanOrEqualTo => ">=",
            ComparisonOperator::EqualTo => "==",
            ComparisonOperator::NotEqualTo => "!=",
        })
    }
}

impl ComparisonOperator {
    pub fn into_score_comparison_operator(self) -> ScoreComparisonOperator {
        match self {
            ComparisonOperator::LessThan => ScoreComparisonOperator::LessThan,
            ComparisonOperator::LessThanOrEqualTo => ScoreComparisonOperator::LessThanOrEqualTo,
            ComparisonOperator::GreaterThan => ScoreComparisonOperator::GreaterThan,
            ComparisonOperator::GreaterThanOrEqualTo => {
                ScoreComparisonOperator::GreaterThanOrEqualTo
            }
            ComparisonOperator::EqualTo | ComparisonOperator::NotEqualTo => {
                ScoreComparisonOperator::EqualTo
            }
        }
    }

    pub fn should_execute_if_be_inverted(&self) -> bool {
        matches!(self, ComparisonOperator::NotEqualTo)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum UnaryOperator {
    Negate,
    Reference,
    Dereference,
    Invert,
}

pub type ExpressionCompoundKind = BTreeMap<HighSNBTString, Expression>;
pub type ConstantExpressionCompoundKind = BTreeMap<HighSNBTString, ConstantExpression>;

fn push_scoreboard_players(
    datapack: &mut HighDatapack,
    ctx: &mut CompileContext,
    command: PlayersScoreboardCommand,
) {
    ctx.add_command(
        datapack,
        Command::Scoreboard(ScoreboardCommand::Players(command)),
    );
}

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

    fn get_data_type(&self) -> DataTypeKind {
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

    pub fn try_as_i32(&self, force: bool) -> Option<i32> {
        Some(match self {
            LiteralExpressionKind::Byte(v) => *v as i32,
            LiteralExpressionKind::Short(v) => *v as i32,
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

    pub fn invert(self) -> Option<LiteralExpressionKind> {
        Some(match self {
            LiteralExpressionKind::Boolean(value) => LiteralExpressionKind::Boolean(!value),
            _ => return None,
        })
    }

    fn compare(
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

    fn to_execute_condition(
        &self,
        datapack: &mut HighDatapack,
    ) -> Option<(bool, ExecuteIfSubcommand)> {
        Some(match self {
            LiteralExpressionKind::Boolean(value) => (
                false,
                ExecuteIfSubcommand::Score(
                    datapack.get_constant_score(1),
                    ScoreComparison::Range(IntegerRange::new_single(if *value { 1 } else { 0 })),
                    None,
                ),
            ),
            _ => return None,
        })
    }

    fn as_text_component(&self, force_display: bool) -> SNBT {
        match self {
            LiteralExpressionKind::Boolean(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::Byte(if *value { 1 } else { 0 })
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
            LiteralExpressionKind::Double(value) => SNBT::Double(*value),
            LiteralExpressionKind::String(string) => SNBT::String(string.snbt_string.clone()),
        }
    }

    fn assign_to_score(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: PlayerScore,
    ) {
        match self {
            Self::Boolean(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, *value as i32),
                );
            }
            Self::Byte(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, *value as i32),
                );
            }
            Self::Short(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, *value as i32),
                );
            }
            Self::Integer(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, *value),
                );
            }
            Self::Long(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, *value as i32),
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

    fn into_snbt(self) -> SNBT {
        match self {
            LiteralExpressionKind::Boolean(value) => SNBT::Byte(if value { 1 } else { 0 }),
            LiteralExpressionKind::Byte(value) => SNBT::Byte(value),
            LiteralExpressionKind::Short(value) => SNBT::Short(value),
            LiteralExpressionKind::Integer(value) => SNBT::Integer(value),
            LiteralExpressionKind::Long(value) => SNBT::Long(value),
            LiteralExpressionKind::Float(value) => SNBT::Float(value),
            LiteralExpressionKind::Double(value) => SNBT::Double(value),
            LiteralExpressionKind::String(snbt_string) => SNBT::String(snbt_string.snbt_string),
        }
    }

    fn cast_to(self, data_type: DataTypeKind) -> Option<LiteralExpressionKind> {
        Some(match (self, data_type) {
            (LiteralExpressionKind::Byte(value), DataTypeKind::Short) => {
                LiteralExpressionKind::Short(value as i16)
            }
            (LiteralExpressionKind::Byte(value), DataTypeKind::Integer) => {
                LiteralExpressionKind::Integer(value as i32)
            }
            (LiteralExpressionKind::Byte(value), DataTypeKind::Long) => {
                LiteralExpressionKind::Long(value as i64)
            }
            (LiteralExpressionKind::Byte(value), DataTypeKind::Float) => {
                LiteralExpressionKind::Float(NotNan::new(value as f32).unwrap())
            }
            (LiteralExpressionKind::Byte(value), DataTypeKind::Double) => {
                LiteralExpressionKind::Double(NotNan::new(value as f64).unwrap())
            }

            (LiteralExpressionKind::Short(value), DataTypeKind::Byte) => {
                LiteralExpressionKind::Byte(value as i8)
            }
            (LiteralExpressionKind::Short(value), DataTypeKind::Integer) => {
                LiteralExpressionKind::Integer(value as i32)
            }
            (LiteralExpressionKind::Short(value), DataTypeKind::Long) => {
                LiteralExpressionKind::Long(value as i64)
            }
            (LiteralExpressionKind::Short(value), DataTypeKind::Float) => {
                LiteralExpressionKind::Float(NotNan::new(value as f32).unwrap())
            }
            (LiteralExpressionKind::Short(value), DataTypeKind::Double) => {
                LiteralExpressionKind::Double(NotNan::new(value as f64).unwrap())
            }

            (LiteralExpressionKind::Integer(value), DataTypeKind::Byte) => {
                LiteralExpressionKind::Byte(value as i8)
            }
            (LiteralExpressionKind::Integer(value), DataTypeKind::Short) => {
                LiteralExpressionKind::Short(value as i16)
            }
            (LiteralExpressionKind::Integer(value), DataTypeKind::Long) => {
                LiteralExpressionKind::Long(value as i64)
            }
            (LiteralExpressionKind::Integer(value), DataTypeKind::Float) => {
                LiteralExpressionKind::Float(NotNan::new(value as f32).unwrap())
            }
            (LiteralExpressionKind::Integer(value), DataTypeKind::Double) => {
                LiteralExpressionKind::Double(NotNan::new(value as f64).unwrap())
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
            _ => return None,
        })
    }

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

    fn perform_arithmetic(
        &self,
        operator: ArithmeticOperator,
        right: LiteralExpressionKind,
    ) -> Option<LiteralExpressionKind> {
        Some(match (self, right) {
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
                    ArithmeticOperator::Swap => return None,
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
                    ArithmeticOperator::Swap => return None,
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
                    ArithmeticOperator::Swap => return None,
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
                    ArithmeticOperator::Swap => return None,
                })
            }

            (LiteralExpressionKind::Float(left), LiteralExpressionKind::Float(right)) => {
                LiteralExpressionKind::Float(match operator {
                    ArithmeticOperator::Add => left + right,
                    ArithmeticOperator::Subtract => left - right,
                    ArithmeticOperator::Multiply => left * right,
                    ArithmeticOperator::FloorDivide => left / right,
                    ArithmeticOperator::Modulo => left % right,
                    _ => return None,
                })
            }

            (LiteralExpressionKind::Double(left), LiteralExpressionKind::Double(right)) => {
                LiteralExpressionKind::Double(match operator {
                    ArithmeticOperator::Add => left + right,
                    ArithmeticOperator::Subtract => left - right,
                    ArithmeticOperator::Multiply => left * right,
                    ArithmeticOperator::FloorDivide => left / right,
                    ArithmeticOperator::Modulo => left % right,
                    _ => return None,
                })
            }

            _ => return None,
        })
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct LiteralExpression {
    #[has_macro(ignore)]
    pub span: ParserRange,
    pub kind: LiteralExpressionKind,
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum ConstantExpressionKind {
    Literal(LiteralExpression),
    Underscore,
    List(Vec<ConstantExpression>),
    Compound(ConstantExpressionCompoundKind),
    PlayerScore(PlayerScore),
    Data(DataTarget, NbtPath),
    Condition(bool, ExecuteIfSubcommand),
    Command(Command),
    Tuple(Vec<ConstantExpression>),
    Dereference(Box<ConstantExpression>),
    Reference(Box<ConstantExpression>),
    Variable(String),
    Unit,
}

impl ConstantExpressionKind {
    pub fn get_pattern_type(&self) -> Option<PatternType> {
        Some(match self {
            ConstantExpressionKind::Literal(literal_expression) => {
                literal_expression.kind.get_pattern_type()
            }
            ConstantExpressionKind::Underscore | ConstantExpressionKind::Variable(_) => {
                PatternType::Any
            }
            _ => return None,
        })
    }

    pub fn dereference(
        &self,
        supports_variable_type_scope: &impl SupportsVariableTypeScope,
    ) -> Option<DataTypeKind> {
        Some(match self {
            ConstantExpressionKind::Reference(expression) => expression
                .kind
                .infer_data_type(supports_variable_type_scope)?,
            ConstantExpressionKind::Variable(name) => {
                return supports_variable_type_scope.get_variable(name)?;
            }
            _ => return None,
        })
    }

    pub fn is_lvalue(&self) -> bool {
        matches!(
            self,
            ConstantExpressionKind::PlayerScore(_)
                | ConstantExpressionKind::Data(_, _)
                | ConstantExpressionKind::Variable(_)
                | ConstantExpressionKind::Dereference(_)
        )
    }

    pub fn can_be_dereferenced(&self) -> bool {
        matches!(
            self,
            ConstantExpressionKind::Reference(_) | ConstantExpressionKind::Variable(_)
        )
    }

    pub fn infer_data_type(
        &self,
        supports_variable_type_scope: &impl SupportsVariableTypeScope,
    ) -> Option<DataTypeKind> {
        Some(match self {
            ConstantExpressionKind::Literal(expression) => expression.kind.get_data_type(),
            ConstantExpressionKind::List(list) => {
                if let Some(first) = list.first() {
                    DataTypeKind::List(Box::new(
                        first.kind.infer_data_type(supports_variable_type_scope)?,
                    ))
                } else {
                    DataTypeKind::List(Box::new(DataTypeKind::Any))
                }
            }
            ConstantExpressionKind::Compound(compound) => DataTypeKind::TypedCompound(
                compound
                    .clone()
                    .into_iter()
                    .map(|(key, value)| {
                        Some((
                            key.snbt_string,
                            value.kind.infer_data_type(supports_variable_type_scope)?,
                        ))
                    })
                    .collect::<Option<_>>()?,
            ),
            ConstantExpressionKind::PlayerScore(_) => DataTypeKind::Score,
            ConstantExpressionKind::Data(_, _) => DataTypeKind::Data(Box::new(DataTypeKind::Any)),
            ConstantExpressionKind::Condition(_, _) => DataTypeKind::Boolean,
            ConstantExpressionKind::Command(_) => DataTypeKind::Integer,
            ConstantExpressionKind::Tuple(expressions) => DataTypeKind::Tuple(
                expressions
                    .iter()
                    .map(|expression| {
                        expression
                            .kind
                            .infer_data_type(supports_variable_type_scope)
                    })
                    .collect::<Option<_>>()?,
            ),
            ConstantExpressionKind::Variable(name) => {
                return supports_variable_type_scope.get_variable(name)?;
            }
            ConstantExpressionKind::Unit => DataTypeKind::Unit,
            ConstantExpressionKind::Dereference(expression) => {
                // TODO
                return expression.kind.dereference(supports_variable_type_scope);
            }
            ConstantExpressionKind::Reference(expression) => DataTypeKind::Reference(Box::new(
                expression
                    .kind
                    .infer_data_type(supports_variable_type_scope)?,
            )),
            ConstantExpressionKind::Underscore => unreachable!(),
        })
    }

    pub fn into_dummy_constant_expression(self) -> ConstantExpression {
        ConstantExpression {
            span: ParserRange::default(),
            kind: self,
        }
    }

    pub fn try_as_i32(&self, force: bool) -> Option<i32> {
        Some(match self {
            ConstantExpressionKind::Literal(expression) => {
                return expression.kind.try_as_i32(force);
            }
            ConstantExpressionKind::List(v) if force => v.len() as i32,
            ConstantExpressionKind::Compound(compound) if force => compound.len() as i32,
            _ => return None,
        })
    }

    pub fn compile_as_statement(&self, datapack: &mut HighDatapack, ctx: &mut CompileContext) {
        match self {
            ConstantExpressionKind::List(constant_expressions) => {
                for constant_expression in constant_expressions {
                    constant_expression.kind.compile_as_statement(datapack, ctx);
                }
            }
            ConstantExpressionKind::Tuple(expressions) => {
                for expression in expressions {
                    expression.kind.compile_as_statement(datapack, ctx);
                }
            }
            ConstantExpressionKind::Compound(compound) => {
                for value in compound.values() {
                    value.kind.compile_as_statement(datapack, ctx);
                }
            }
            ConstantExpressionKind::Condition(inverted, condition) => {
                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::If(*inverted, condition.clone())),
                );
            }
            ConstantExpressionKind::Command(command) => {
                ctx.add_command(datapack, command.clone());
            }
            ConstantExpressionKind::Literal(_)
            | ConstantExpressionKind::PlayerScore(_)
            | ConstantExpressionKind::Data(_, _)
            | ConstantExpressionKind::Unit => {}
            ConstantExpressionKind::Reference(expression) => {
                expression.kind.compile_as_statement(datapack, ctx)
            }
            ConstantExpressionKind::Dereference(expression) => {
                expression.kind.compile_as_statement(datapack, ctx)
            }
            ConstantExpressionKind::Variable(name) => datapack
                .get_variable(name)
                .unwrap()
                .1
                .kind
                .compile_as_statement(datapack, ctx),
            ConstantExpressionKind::Underscore => unreachable!(),
        }
    }

    #[must_use]
    pub fn as_score(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        force: bool,
    ) -> PlayerScore {
        if let Some(value) = self.try_as_i32(force) {
            return if force {
                let unique_score = datapack.get_unique_player_score();
                self.assign_to_score(datapack, ctx, unique_score.clone());
                unique_score
            } else {
                datapack.get_constant_score(value)
            };
        }

        match self {
            ConstantExpressionKind::PlayerScore(player_score) if !force => player_score.clone(),
            _ => {
                let unique_score = datapack.get_unique_player_score();
                self.assign_to_score(datapack, ctx, unique_score.clone());
                unique_score
            }
        }
    }

    #[must_use]
    pub fn as_data(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> (DataTarget, NbtPath) {
        match self {
            ConstantExpressionKind::Data(target, path) => (target.clone(), path.clone()),
            _ => {
                let (unique_data, path) = datapack.get_unique_data();

                self.assign_to_data(datapack, ctx, unique_data.clone(), path.clone());

                (unique_data, path)
            }
        }
    }

    #[must_use]
    pub fn as_data_force(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> (DataTarget, NbtPath) {
        let (unique_data, path) = datapack.get_unique_data();

        self.assign_to_data(datapack, ctx, unique_data.clone(), path.clone());

        (unique_data, path)
    }

    pub fn invert(self) -> Option<ConstantExpressionKind> {
        Some(match self {
            ConstantExpressionKind::Literal(expression) => {
                ConstantExpressionKind::Literal(LiteralExpression {
                    span: expression.span,
                    kind: expression.kind.invert()?,
                })
            }
            ConstantExpressionKind::Condition(inverted, subcommand) => {
                ConstantExpressionKind::Condition(!inverted, subcommand)
            }
            _ => return None,
        })
    }

    pub fn index(self, index: ConstantExpressionKind) -> Option<ConstantExpressionKind> {
        Some(match self {
            ConstantExpressionKind::List(mut items) => {
                return if let ConstantExpressionKind::Literal(LiteralExpression {
                    kind: LiteralExpressionKind::Integer(index),
                    ..
                }) = index
                {
                    if index >= 0 && (index as usize) < items.len() {
                        Some(items.swap_remove(index as usize).kind)
                    } else {
                        None
                    }
                } else {
                    None
                };
            }
            ConstantExpressionKind::Data(target, path) => {
                if let Some(snbt) = index.as_snbt() {
                    ConstantExpressionKind::Data(
                        target,
                        path.with_node(NbtPathNode::Index(Some(snbt))),
                    )
                } else {
                    return None;
                }
            }
            _ => return None,
        })
    }

    pub fn access_member(self, member: SNBTString) -> Option<ConstantExpressionKind> {
        Some(match self {
            ConstantExpressionKind::Literal(_)
            | ConstantExpressionKind::List(_)
            | ConstantExpressionKind::PlayerScore(_)
            | ConstantExpressionKind::Condition(_, _)
            | ConstantExpressionKind::Command(_)
            | ConstantExpressionKind::Unit
            | ConstantExpressionKind::Variable(_)
            | ConstantExpressionKind::Reference(_)
            | ConstantExpressionKind::Dereference(_) => return None,
            ConstantExpressionKind::Underscore => unreachable!(),
            ConstantExpressionKind::Compound(compound) => {
                compound
                    .into_iter()
                    .find(|(key, _)| key.snbt_string == member)
                    .map(|(_, value)| value)
                    .unwrap()
                    .kind
            }
            ConstantExpressionKind::Data(target, path) => ConstantExpressionKind::Data(
                target,
                path.with_node(NbtPathNode::Named(member, None)),
            ),
            ConstantExpressionKind::Tuple(mut expressions) => {
                if let Ok(index) = member.1.parse::<i32>() {
                    expressions.remove(index as usize).kind
                } else {
                    return None;
                }
            }
        })
    }

    #[inline]
    pub fn is_data(&self) -> bool {
        matches!(self, ConstantExpressionKind::Data(_, _))
    }

    pub fn as_data_command_modification(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> DataCommandModification {
        if let Some(snbt) = self.as_snbt() {
            return DataCommandModification::Value(snbt);
        }

        let (target, path) = self.as_data(datapack, ctx);

        DataCommandModification::From(target, Some(path))
    }

    pub fn compare(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        other: ConstantExpressionKind,
        operator: ComparisonOperator,
    ) -> ConstantExpressionKind {
        match (self, other) {
            (ConstantExpressionKind::Literal(left), ConstantExpressionKind::Literal(right)) => {
                ConstantExpressionKind::Literal(LiteralExpression {
                    span: ParserRange { start: 0, end: 0 },
                    kind: left.kind.compare(operator, right.kind).unwrap(),
                })
            }
            (left_kind @ ConstantExpressionKind::Data(_, _), right_kind)
                if operator == ComparisonOperator::EqualTo
                    || operator == ComparisonOperator::NotEqualTo =>
            {
                let unique_score = datapack.get_unique_player_score();

                let (unique_target, unique_path) = datapack.get_unique_data();

                left_kind.assign_to_data(datapack, ctx, unique_target.clone(), unique_path.clone());

                let data_command_modification =
                    right_kind.as_data_command_modification(datapack, ctx);

                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Success,
                        ExecuteStoreSubcommand::Score(
                            unique_score.clone(),
                            Box::new(ExecuteSubcommand::Run(Box::new(Command::Data(
                                DataCommand::Modify(
                                    unique_target,
                                    unique_path,
                                    DataCommandModificationMode::Set,
                                    data_command_modification,
                                ),
                            )))),
                        ),
                    )),
                );

                ConstantExpressionKind::Condition(
                    operator.should_execute_if_be_inverted(),
                    ExecuteIfSubcommand::Score(
                        unique_score,
                        ScoreComparison::Range(IntegerRange::new_single(0)),
                        None,
                    ),
                )
            }
            (ConstantExpressionKind::PlayerScore(left_score), right_kind) => {
                let right_score = right_kind.as_score(datapack, ctx, false);

                ConstantExpressionKind::Condition(
                    operator.should_execute_if_be_inverted(),
                    ExecuteIfSubcommand::Score(
                        left_score,
                        ScoreComparison::Score(
                            operator.into_score_comparison_operator(),
                            right_score,
                        ),
                        None,
                    ),
                )
            }
            (left_kind, ConstantExpressionKind::PlayerScore(right_score)) => {
                let left_score = left_kind.as_score(datapack, ctx, false);

                ConstantExpressionKind::Condition(
                    operator.should_execute_if_be_inverted(),
                    ExecuteIfSubcommand::Score(
                        right_score,
                        ScoreComparison::Score(
                            operator.into_score_comparison_operator(),
                            left_score,
                        ),
                        None,
                    ),
                )
            }
            _ => unreachable!(),
        }
    }

    pub fn operate_on_score(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: &PlayerScore,
        operator: ArithmeticOperator,
    ) {
        if let Some(value) = self.try_as_i32(true) {
            return match operator {
                ArithmeticOperator::Add => {
                    push_scoreboard_players(
                        datapack,
                        ctx,
                        PlayersScoreboardCommand::Add(target.clone(), value),
                    );
                }
                ArithmeticOperator::Subtract => {
                    push_scoreboard_players(
                        datapack,
                        ctx,
                        PlayersScoreboardCommand::Remove(target.clone(), value),
                    );
                }
                ArithmeticOperator::And => {
                    datapack
                        .get_constant_score(value)
                        .operate_on_score(datapack, ctx, target, operator);
                }
                ArithmeticOperator::Or => {
                    datapack
                        .get_constant_score(value)
                        .operate_on_score(datapack, ctx, target, operator);
                }
                ArithmeticOperator::LeftShift => {
                    compile_shift_operation(
                        datapack,
                        ctx,
                        target,
                        value,
                        ScoreOperationOperator::Multiply,
                    );
                }
                ArithmeticOperator::RightShift => {
                    compile_shift_operation(
                        datapack,
                        ctx,
                        target,
                        value,
                        ScoreOperationOperator::Divide,
                    );
                }
                _ => {
                    let constant_score = datapack.get_constant_score(value);

                    push_scoreboard_players(
                        datapack,
                        ctx,
                        PlayersScoreboardCommand::Operation(
                            target.clone(),
                            operator
                                .into_scoreboard_players_operation_operator()
                                .unwrap(),
                            constant_score,
                        ),
                    );
                }
            };
        }

        match self {
            ConstantExpressionKind::PlayerScore(source) => {
                source
                    .clone()
                    .operate_on_score(datapack, ctx, target, operator);
            }
            _ => {
                let unique_score = datapack.get_unique_player_score();

                self.assign_to_score(datapack, ctx, unique_score.clone());

                unique_score
                    .clone()
                    .operate_on_score(datapack, ctx, target, operator);
            }
        }
    }

    pub fn to_execute_condition(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        inverted: bool,
    ) -> (bool, ExecuteIfSubcommand) {
        if let Some(value) = self.try_as_i32(true) {
            let unique_score = datapack.get_unique_player_score();

            push_scoreboard_players(
                datapack,
                ctx,
                PlayersScoreboardCommand::Set(unique_score.clone(), value),
            );

            return (
                !inverted,
                ExecuteIfSubcommand::Score(
                    unique_score,
                    ScoreComparison::Range(IntegerRange::new_single(0)),
                    None,
                ),
            );
        }

        match self {
            ConstantExpressionKind::Literal(expression) => {
                expression.kind.to_execute_condition(datapack).unwrap()
            }
            ConstantExpressionKind::PlayerScore(score) => (
                !inverted,
                ExecuteIfSubcommand::Score(
                    score.clone(),
                    ScoreComparison::Range(IntegerRange::new_single(0)),
                    None,
                ),
            ),
            ConstantExpressionKind::Data(target, path) => (
                inverted,
                ExecuteIfSubcommand::Data(target.clone(), path.clone(), None),
            ),
            ConstantExpressionKind::Condition(inner_inverted, condition) => {
                (inverted ^ *inner_inverted, condition.clone())
            }
            ConstantExpressionKind::Command(command) => {
                let unique_score = datapack.get_unique_player_score();

                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Success,
                        ExecuteStoreSubcommand::Score(
                            unique_score.clone(),
                            Box::new(ExecuteSubcommand::Run(Box::new(command.clone()))),
                        ),
                    )),
                );

                (
                    !inverted,
                    ExecuteIfSubcommand::Score(
                        unique_score,
                        ScoreComparison::Range(IntegerRange::new_single(0)),
                        None,
                    ),
                )
            }
            ConstantExpressionKind::Variable(name) => datapack
                .get_variable(name)
                .unwrap()
                .1
                .kind
                .to_execute_condition(datapack, ctx, inverted),
            ConstantExpressionKind::List(_)
            | ConstantExpressionKind::Compound(_)
            | ConstantExpressionKind::Tuple(_)
            | ConstantExpressionKind::Unit
            | ConstantExpressionKind::Reference(_)
            | ConstantExpressionKind::Dereference(_)
            | ConstantExpressionKind::Underscore => {
                unreachable!()
            }
        }
    }

    pub fn as_text_component(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        force_display: bool,
    ) -> SNBT {
        match self {
            ConstantExpressionKind::PlayerScore(player_score) => {
                player_score.clone().to_text_component()
            }
            ConstantExpressionKind::Data(target, path) => {
                let mut map = BTreeMap::new();

                match target {
                    DataTarget::Block(coordinates) => {
                        map.insert(
                            SNBTString(false, "block".to_string()),
                            SNBT::string(coordinates),
                        );
                    }
                    DataTarget::Entity(entity_selector) => {
                        map.insert(
                            SNBTString(false, "entity".to_string()),
                            SNBT::string(entity_selector),
                        );
                    }
                    DataTarget::Storage(resource_location) => {
                        map.insert(
                            SNBTString(false, "storage".to_string()),
                            SNBT::string(resource_location),
                        );
                    }
                }

                map.insert(SNBTString(false, "nbt".to_string()), path.to_snbt_string());

                SNBT::Compound(map)
            }
            ConstantExpressionKind::Literal(expression) => {
                expression.kind.as_text_component(force_display)
            }
            ConstantExpressionKind::List(constant_expressions) => SNBT::List(
                constant_expressions
                    .iter()
                    .map(|expression| expression.kind.as_text_component(datapack, ctx, false))
                    .collect(),
            ),
            ConstantExpressionKind::Compound(btree_map) => SNBT::Compound(
                btree_map
                    .iter()
                    .map(|(key, value)| {
                        (
                            key.snbt_string.clone(),
                            value.kind.as_text_component(datapack, ctx, false),
                        )
                    })
                    .collect(),
            ),
            ConstantExpressionKind::Condition(_, _) => {
                let unique_ecore = datapack.get_unique_player_score();

                self.assign_to_score(datapack, ctx, unique_ecore.clone());

                unique_ecore.clone().to_text_component()
            }
            ConstantExpressionKind::Command(_) => {
                let unique_score = datapack.get_unique_player_score();

                self.assign_to_score(datapack, ctx, unique_score.clone());

                unique_score.clone().to_text_component()
            }
            ConstantExpressionKind::Tuple(expressions) => {
                let mut items = Vec::new();

                items.push(SNBT::string("("));

                for (i, expression) in expressions.iter().enumerate() {
                    if i != 0 {
                        items.push(SNBT::string(", "));
                    }

                    items.push(expression.kind.as_text_component(datapack, ctx, true));
                }

                items.push(SNBT::string(")"));

                SNBT::List(items)
            }
            ConstantExpressionKind::Unit => SNBT::string("()"),
            ConstantExpressionKind::Reference(expression) => {
                expression
                    .kind
                    .as_text_component(datapack, ctx, force_display)
            }
            ConstantExpressionKind::Dereference(expression) => {
                expression
                    .kind
                    .as_text_component(datapack, ctx, force_display)
            }
            ConstantExpressionKind::Variable(name) => datapack
                .get_variable(name)
                .unwrap()
                .1
                .kind
                .as_text_component(datapack, ctx, force_display),
            ConstantExpressionKind::Underscore => unreachable!(),
        }
    }

    pub fn assign_to_score(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: PlayerScore,
    ) {
        match self {
            ConstantExpressionKind::Literal(expression) => {
                expression.kind.assign_to_score(datapack, ctx, target)
            }
            ConstantExpressionKind::List(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, value.len() as i32),
                );
            }
            ConstantExpressionKind::Compound(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, value.len() as i32),
                );
            }
            ConstantExpressionKind::PlayerScore(source) => {
                source.clone().assign_to_score(datapack, ctx, target);
            }
            ConstantExpressionKind::Data(data_target, path) => {
                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Result,
                        ExecuteStoreSubcommand::Score(
                            target,
                            Box::new(ExecuteSubcommand::Run(Box::new(Command::Data(
                                DataCommand::Get(data_target.clone(), Some(path.clone()), None),
                            )))),
                        ),
                    )),
                );
            }
            ConstantExpressionKind::Condition(inverted, condition) => {
                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Success,
                        ExecuteStoreSubcommand::Score(
                            target,
                            Box::new(ExecuteSubcommand::If(*inverted, condition.clone())),
                        ),
                    )),
                );
            }
            ConstantExpressionKind::Command(command) => {
                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Result,
                        ExecuteStoreSubcommand::Score(
                            target,
                            Box::new(ExecuteSubcommand::Run(Box::new(command.clone()))),
                        ),
                    )),
                );
            }
            ConstantExpressionKind::Tuple(_) => unreachable!(),
            ConstantExpressionKind::Unit => unreachable!(),
            ConstantExpressionKind::Reference(expression) => {
                expression.kind.assign_to_score(datapack, ctx, target)
            }
            ConstantExpressionKind::Dereference(_) => unreachable!(),
            ConstantExpressionKind::Variable(name) => datapack
                .get_variable(name)
                .unwrap()
                .1
                .kind
                .assign_to_score(datapack, ctx, target),
            ConstantExpressionKind::Underscore => unreachable!(),
        }
    }

    pub fn assign_to_data(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: DataTarget,
        path: NbtPath,
    ) {
        if let Some(value) = self.as_snbt() {
            ctx.add_command(
                datapack,
                Command::Data(DataCommand::Modify(
                    target,
                    path,
                    DataCommandModificationMode::Set,
                    DataCommandModification::Value(value),
                )),
            );

            return;
        }

        match self {
            ConstantExpressionKind::PlayerScore(score) => {
                score.clone().assign_to_data(datapack, ctx, target, path);
            }
            ConstantExpressionKind::Data(inner_target, inner_path) => {
                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Modify(
                        target,
                        path,
                        DataCommandModificationMode::Set,
                        DataCommandModification::From(
                            inner_target.clone(),
                            Some(inner_path.clone()),
                        ),
                    )),
                );
            }
            ConstantExpressionKind::List(list) => {
                let (constants, non_constants) = split_constants_list(list);

                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Modify(
                        target.clone(),
                        path.clone(),
                        DataCommandModificationMode::Set,
                        DataCommandModification::Value(SNBT::List(constants)),
                    )),
                );

                for (index, non_constant) in non_constants {
                    non_constant.kind.assign_to_data(
                        datapack,
                        ctx,
                        target.clone(),
                        path.clone()
                            .with_node(NbtPathNode::Index(Some(SNBT::Integer(index as i32)))),
                    );
                }
            }
            ConstantExpressionKind::Compound(compound) => {
                let (constants, non_constants) = split_constants_compound(compound);

                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Modify(
                        target.clone(),
                        path.clone(),
                        DataCommandModificationMode::Set,
                        DataCommandModification::Value(SNBT::Compound(constants)),
                    )),
                );

                for (key, non_constant) in non_constants {
                    non_constant.kind.assign_to_data(
                        datapack,
                        ctx,
                        target.clone(),
                        path.clone().with_node(NbtPathNode::named(key.snbt_string)),
                    );
                }
            }
            ConstantExpressionKind::Unit => {
                let mut unit_btreemap = BTreeMap::new();
                unit_btreemap.insert(
                    SNBTString(false, "__kelp_rs_unit__".to_string()),
                    SNBT::Byte(1),
                );

                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Modify(
                        target.clone(),
                        path.clone(),
                        DataCommandModificationMode::Set,
                        DataCommandModification::Value(SNBT::Compound(unit_btreemap)),
                    )),
                );
            }
            ConstantExpressionKind::Reference(expression) => {
                expression.kind.assign_to_data(datapack, ctx, target, path)
            }
            _ => unreachable!(),
        }
    }

    pub fn as_snbt_macros(self, ctx: &mut CompileContext) -> SNBT {
        match self {
            ConstantExpressionKind::Literal(expression) => expression.kind.into_snbt(),
            ConstantExpressionKind::List(expressions) => SNBT::List(
                expressions
                    .clone()
                    .into_iter()
                    .map(|expression| expression.kind.as_snbt_macros(ctx))
                    .collect(),
            ),
            ConstantExpressionKind::Compound(compound) => SNBT::Compound(
                compound
                    .clone()
                    .into_iter()
                    .map(|(key, value)| (key.snbt_string, value.kind.as_snbt_macros(ctx)))
                    .collect(),
            ),
            _ => ctx.get_macro_snbt(self),
        }
    }

    pub fn as_snbt(&self) -> Option<SNBT> {
        match self {
            ConstantExpressionKind::Literal(expression) => {
                Some(expression.clone().kind.into_snbt())
            }

            ConstantExpressionKind::List(expressions) => expressions
                .iter()
                .map(|expr| expr.kind.as_snbt())
                .collect::<Option<Vec<_>>>()
                .map(SNBT::List),

            ConstantExpressionKind::Compound(compound) => compound
                .iter()
                .map(|(key, value)| value.kind.as_snbt().map(|v| (key.snbt_string.clone(), v)))
                .collect::<Option<_>>()
                .map(SNBT::Compound),

            _ => None,
        }
    }

    fn cast_to(
        self,
        datapack: &mut HighDatapack,
        data_type: DataTypeKind,
    ) -> Option<ConstantExpressionKind> {
        let self_type = self.infer_data_type(datapack).unwrap();

        if self_type.equals(&data_type) {
            return Some(self);
        }

        Some(match (self, data_type) {
            (ConstantExpressionKind::Literal(expression), data_type) => {
                return expression.kind.cast_to(data_type).map(|kind| {
                    ConstantExpressionKind::Literal(LiteralExpression {
                        span: expression.span,
                        kind,
                    })
                });
            }

            (self_ @ ConstantExpressionKind::Data(_, _), DataTypeKind::Data(_)) => self_,

            _ => return None,
        })
    }

    pub fn assign(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        value: ConstantExpression,
    ) {
        match self {
            ConstantExpressionKind::PlayerScore(score) => {
                value.kind.assign_to_score(datapack, ctx, score);
            }
            ConstantExpressionKind::Data(target, path) => {
                value.kind.assign_to_data(datapack, ctx, target, path);
            }
            ConstantExpressionKind::Variable(name) => {
                datapack.assign_variable(&name, value);
            }
            _ => unreachable!("The expression '{:?}' cannot be assigned to", self),
        }
    }

    pub fn as_place(self) -> Option<Place> {
        Some(match self {
            ConstantExpressionKind::PlayerScore(score) => Place::Score(score),
            ConstantExpressionKind::Data(target, path) => Place::Data(target, path),
            ConstantExpressionKind::Variable(name) => Place::Variable(name),
            ConstantExpressionKind::Underscore => Place::Underscore,
            ConstantExpressionKind::Tuple(expressions) => Place::Tuple(
                expressions
                    .into_iter()
                    .map(|expression| expression.kind.as_place())
                    .collect::<Option<_>>()?,
            ),
            _ => return None,
        })
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct ConstantExpression {
    #[has_macro(ignore)]
    pub span: ParserRange,
    pub kind: ConstantExpressionKind,
}

impl ConstantExpression {
    pub fn place_type(&self, ctx: &impl SupportsVariableTypeScope) -> Option<PlaceType> {
        Some(match &self.kind {
            ConstantExpressionKind::PlayerScore(_) => PlaceType::Score,
            ConstantExpressionKind::Data(_, _) => PlaceType::Data,
            ConstantExpressionKind::Dereference(expression) => return expression.place_type(ctx),
            ConstantExpressionKind::Variable(name) => PlaceType::Variable(ctx.get_variable(name)??),
            ConstantExpressionKind::Underscore => PlaceType::Underscore,
            _ => return None,
        })
    }

    pub fn try_dereference(self, datapack: &mut HighDatapack) -> Option<ConstantExpression> {
        Some(match self.kind {
            ConstantExpressionKind::Variable(name) => datapack.get_variable(&name).unwrap().1,
            ConstantExpressionKind::Reference(expression) => *expression,
            ConstantExpressionKind::PlayerScore(_) | ConstantExpressionKind::Data(_, _) => self,
            _ => return None,
        })
    }

    pub fn resolve(self, datapack: &mut HighDatapack) -> ConstantExpression {
        match self.kind {
            ConstantExpressionKind::Variable(name) => datapack.get_variable(&name).unwrap().1,
            _ => self,
        }
    }

    #[must_use]
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match &self.kind {
            ConstantExpressionKind::Literal(expression) => {
                expression.kind.perform_semantic_analysis(ctx, is_lhs)
            }
            ConstantExpressionKind::Unit
            | ConstantExpressionKind::PlayerScore(_)
            | ConstantExpressionKind::Data(_, _) => Some(()),
            ConstantExpressionKind::Reference(expression) => {
                let data_type = expression.kind.infer_data_type(ctx)?;

                if !expression.kind.is_lvalue() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: expression.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeReferenced(data_type),
                        ),
                    });
                }

                Some(())
            }
            ConstantExpressionKind::Dereference(expression) => {
                let data_type = expression.kind.infer_data_type(ctx)?;

                if !expression.kind.can_be_dereferenced() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: expression.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeDereferenced(data_type),
                        ),
                    });
                }

                Some(())
            }
            ConstantExpressionKind::Variable(name) => {
                if !ctx.variable_is_declared(name) {
                    ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::UndeclaredVariable(name.clone()),
                        ),
                    })
                } else {
                    Some(())
                }
            }
            ConstantExpressionKind::List(list) => list
                .iter()
                .map(|item| item.perform_semantic_analysis(ctx, is_lhs))
                .collect::<Option<()>>(),
            ConstantExpressionKind::Compound(compound) => compound
                .values()
                .map(|value| value.perform_semantic_analysis(ctx, is_lhs))
                .collect::<Option<()>>(),
            ConstantExpressionKind::Condition(_, _) => {
                // TODO future

                Some(())
            }
            ConstantExpressionKind::Command(_command) => {
                // TODO future

                Some(())
            }
            ConstantExpressionKind::Tuple(items) => items
                .iter()
                .map(|item| item.perform_semantic_analysis(ctx, is_lhs))
                .collect::<Option<()>>(),
            ConstantExpressionKind::Underscore => {
                if !is_lhs {
                    ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::UnderscoreExpression,
                        ),
                    })
                } else {
                    Some(())
                }
            }
        }
    }

    pub fn map_kind(
        self,
        f: impl FnOnce(ConstantExpressionKind) -> ConstantExpressionKind,
    ) -> ConstantExpression {
        Self {
            kind: f(self.kind),
            ..self
        }
    }

    pub fn into_constant_expression(self) -> Expression {
        Expression {
            span: self.span,
            kind: ExpressionKind::Constant(self),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum StringExpressionPart {
    Regular(String),
    Expression(Box<Expression>),
}

impl HasMacro for StringExpressionPart {
    fn has_macro(&self) -> bool {
        matches!(self, StringExpressionPart::Expression(_))
    }

    fn has_macro_conflict(&self) -> bool {
        false
    }
}

impl StringExpressionPart {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            StringExpressionPart::Regular(_) => Some(()),
            StringExpressionPart::Expression(expression) => {
                expression.perform_semantic_analysis(ctx, is_lhs)
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct HighSNBTString {
    pub span: ParserRange,
    pub snbt_string: SNBTString,
}

impl_has_macro_false!(HighSNBTString);

impl From<String> for HighSNBTString {
    fn from(value: String) -> Self {
        HighSNBTString {
            span: ParserRange { start: 0, end: 0 },
            snbt_string: SNBTString(false, value),
        }
    }
}

impl HighSNBTString {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        _is_lhs: bool,
    ) -> Option<()> {
        if self.snbt_string.has_macro_conflict() {
            ctx.add_info(SemanticAnalysisInfo {
                span: self.span,
                kind: SemanticAnalysisInfoKind::Error(SemanticAnalysisError::MacroConflict),
            })
        } else {
            Some(())
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum StringExpression {
    Simple(String),
    Complex(Vec<StringExpressionPart>),
}

impl HasMacro for StringExpression {
    fn has_macro(&self) -> bool {
        match self {
            StringExpression::Simple(_) => false,
            StringExpression::Complex(parts) => parts.iter().any(|part| part.has_macro()),
        }
    }

    fn has_macro_conflict(&self) -> bool {
        false
    }
}

impl From<String> for StringExpression {
    fn from(value: String) -> Self {
        Self::Simple(value)
    }
}

impl StringExpression {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            StringExpression::Simple(_) => Some(()),
            StringExpression::Complex(parts) => parts
                .iter()
                .map(|part| part.perform_semantic_analysis(ctx, is_lhs))
                .all_some(),
        }
    }

    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> SNBTString {
        match self {
            StringExpression::Simple(contents) => SNBTString(false, contents),
            StringExpression::Complex(parts) => {
                let mut has_macro = false;
                let mut output = String::new();

                for part in parts {
                    match part {
                        StringExpressionPart::Regular(s) => output.push_str(&s),
                        StringExpressionPart::Expression(expression) => {
                            let expr = expression.resolve(datapack, ctx).kind.as_snbt_macros(ctx);

                            output.push_str(&expr.to_string());

                            has_macro = true;
                        }
                    }
                }

                SNBTString(has_macro, output)
            }
        }
    }
}

pub trait SupportsVariableTypeScope {
    fn get_variable(&self, name: &str) -> Option<Option<DataTypeKind>>;

    fn add_info(&mut self, semantic_analysis_info: SemanticAnalysisInfo);
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, HasMacro)]
pub enum ExpressionKind {
    Constant(ConstantExpression),
    Unary(UnaryOperator, Box<Expression>),
    Arithmetic(Box<Expression>, ArithmeticOperator, Box<Expression>),
    Comparison(Box<Expression>, ComparisonOperator, Box<Expression>),
    Logical(Box<Expression>, LogicalOperator, Box<Expression>),
    AugmentedAssignment(Box<Expression>, ArithmeticOperator, Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>),
    List(Vec<Expression>),
    Compound(ExpressionCompoundKind),
    PlayerScore(HighPlayerScore),
    Data(HighDataTarget, HighNbtPath),
    Condition(bool, HighExecuteIfSubcommand),
    Command(Box<HighCommand>),
    Index(Box<Expression>, Box<Expression>),
    FieldAccess(Box<Expression>, HighSNBTString),
    AsCast(Box<Expression>, HighDataType),
    ToCast(Box<Expression>, RuntimeStorageType),
    Tuple(Vec<Expression>),
    // TODO ByteArray(Vec<i8>),
    // TODO IntegerArray(Vec<i32>),
    // TODO LongArray(Vec<i64>),
}

impl ExpressionKind {
    pub fn is_lvalue(&self) -> bool {
        match self {
            ExpressionKind::Constant(expression) => expression.kind.is_lvalue(),
            ExpressionKind::Unary(UnaryOperator::Dereference, _) => true,
            ExpressionKind::PlayerScore(_) => true,
            ExpressionKind::Data(_, _) => true,
            ExpressionKind::Index(_, _) => true,
            ExpressionKind::FieldAccess(_, _) => true,
            ExpressionKind::AsCast(_, high_data_type) => high_data_type.kind.resolve().is_lvalue(),
            ExpressionKind::ToCast(_, _) => true,
            _ => false,
        }
    }

    pub fn can_be_dereferenced(&self) -> bool {
        match self {
            ExpressionKind::Constant(expression) => expression.kind.can_be_dereferenced(),
            ExpressionKind::Unary(UnaryOperator::Reference, _) => true,
            ExpressionKind::Unary(UnaryOperator::Dereference, _) => true,
            _ => false,
        }
    }

    pub fn infer_data_type(
        &self,
        supports_variable_type_scope: &mut impl SupportsVariableTypeScope,
    ) -> Option<DataTypeKind> {
        Some(match self {
            ExpressionKind::Constant(constant_expression) => {
                return constant_expression
                    .kind
                    .infer_data_type(supports_variable_type_scope);
            }
            ExpressionKind::Unary(operator, expression) => {
                let expression_type = expression
                    .kind
                    .infer_data_type(supports_variable_type_scope)?;

                match operator {
                    UnaryOperator::Negate => return expression_type.get_negated_result(),
                    UnaryOperator::Reference => DataTypeKind::Reference(Box::new(expression_type)),
                    UnaryOperator::Dereference => expression
                        .dereference_type(supports_variable_type_scope)
                        .unwrap(),
                    UnaryOperator::Invert => return expression_type.get_inverted_result(),
                }
            }
            ExpressionKind::Arithmetic(left, operator, right) => {
                let left_type = left.kind.infer_data_type(supports_variable_type_scope)?;
                let right_type = right.kind.infer_data_type(supports_variable_type_scope)?;

                return left_type.get_arithmetic_result(operator, &right_type);
            }
            ExpressionKind::Comparison(_, _, _) | ExpressionKind::Logical(_, _, _) => {
                DataTypeKind::Boolean
            }
            ExpressionKind::Assignment(_, _) | ExpressionKind::AugmentedAssignment(_, _, _) => {
                DataTypeKind::Unit
            }
            ExpressionKind::List(list) => {
                let data_type = if let Some(first) = list.first() {
                    first.kind.infer_data_type(supports_variable_type_scope)?
                } else {
                    DataTypeKind::Any
                };

                DataTypeKind::List(Box::new(data_type))
            }
            ExpressionKind::Compound(compound) => DataTypeKind::TypedCompound(
                compound
                    .clone()
                    .into_iter()
                    .map(|(key, value)| {
                        value
                            .kind
                            .infer_data_type(supports_variable_type_scope)
                            .map(|data_type| (key.snbt_string, data_type))
                    })
                    .collect::<Option<_>>()?,
            ),
            ExpressionKind::PlayerScore(_) => DataTypeKind::Score,
            ExpressionKind::Data(_, _) => DataTypeKind::Data(Box::new(DataTypeKind::Any)),
            ExpressionKind::Condition(_, _) => DataTypeKind::Byte,
            ExpressionKind::Command(_) => DataTypeKind::Integer,
            ExpressionKind::Index(target, _) => {
                return target
                    .kind
                    .infer_data_type(supports_variable_type_scope)?
                    .get_index_result();
            }
            ExpressionKind::FieldAccess(target, field) => {
                let target = target.kind.infer_data_type(supports_variable_type_scope)?;

                return target.get_field_result(&field.snbt_string);
            }
            ExpressionKind::AsCast(_, data_type) => data_type.kind.resolve(),
            ExpressionKind::ToCast(expression, storage_type) => match storage_type {
                RuntimeStorageType::Score => DataTypeKind::Score,
                RuntimeStorageType::Data => {
                    let expression_type = expression
                        .kind
                        .infer_data_type(supports_variable_type_scope)?;

                    DataTypeKind::Data(Box::new(expression_type))
                }
            },
            ExpressionKind::Tuple(expressions) => DataTypeKind::Tuple(
                expressions
                    .iter()
                    .map(|expression| {
                        expression
                            .kind
                            .infer_data_type(supports_variable_type_scope)
                    })
                    .collect::<Option<_>>()?,
            ),
        })
    }
}

impl ExpressionKind {
    pub fn into_dummy_expression(self) -> Expression {
        Expression {
            span: ParserRange::default(),
            kind: self,
        }
    }
}

pub fn split_constants_list(
    list: &[ConstantExpression],
) -> (Vec<SNBT>, Vec<(usize, ConstantExpression)>) {
    let mut constants = Vec::new();
    let mut non_constants = Vec::new();

    for (i, expression) in list.iter().enumerate() {
        if let Some(snbt) = expression.kind.as_snbt() {
            constants.push(snbt);
        } else {
            non_constants.push((i, expression.clone()));
            constants.push(SNBT::Compound(BTreeMap::new()));
        }
    }

    (constants, non_constants)
}

pub fn split_constants_compound(
    compound: &ConstantExpressionCompoundKind,
) -> (SNBTCompound, ConstantExpressionCompoundKind) {
    let mut constants = BTreeMap::new();
    let mut non_constants = BTreeMap::new();

    for (key, expression) in compound.iter() {
        if let Some(snbt) = expression.kind.as_snbt() {
            constants.insert(key.snbt_string.clone(), snbt);
        } else {
            non_constants.insert(key.clone(), expression.clone());
            constants.insert(key.snbt_string.clone(), SNBT::Compound(BTreeMap::new()));
        }
    }

    (constants, non_constants)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, HasMacro)]
pub struct Expression {
    #[has_macro(ignore)]
    pub span: ParserRange,
    pub kind: ExpressionKind,
}

impl Expression {
    pub fn as_place(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> Option<Place> {
        Some(match self.kind {
            ExpressionKind::Constant(expression) => return expression.kind.as_place(),
            ExpressionKind::PlayerScore(score) => {
                let score = score.compile(datapack, ctx);

                Place::Score(score)
            }
            ExpressionKind::Data(target, path) => {
                let target = target.kind.compile(datapack, ctx);
                let path = path.compile(datapack, ctx);

                Place::Data(target, path)
            }
            ExpressionKind::Tuple(expressions) => Place::Tuple(
                expressions
                    .into_iter()
                    .map(|expression| expression.as_place(datapack, ctx))
                    .collect::<Option<_>>()?,
            ),
            ExpressionKind::Unary(UnaryOperator::Dereference, expression) => {
                expression.dereference(datapack)?.as_place(datapack, ctx)?
            }
            _ => return None,
        })
    }

    pub fn dereference(self, datapack: &mut HighDatapack) -> Option<Expression> {
        Some(match self.kind {
            ExpressionKind::Constant(expression) => match expression.kind {
                ConstantExpressionKind::Variable(name) => datapack
                    .get_variable(&name)
                    .unwrap()
                    .1
                    .into_constant_expression()
                    .dereference(datapack)?,
                ConstantExpressionKind::Reference(expression) => {
                    expression.into_constant_expression()
                }
                kind @ ConstantExpressionKind::PlayerScore(_) => Expression {
                    span: self.span,
                    kind: ExpressionKind::Constant(ConstantExpression {
                        span: self.span,
                        kind,
                    }),
                },
                kind @ ConstantExpressionKind::Data(_, _) => Expression {
                    span: self.span,
                    kind: ExpressionKind::Constant(ConstantExpression {
                        span: self.span,
                        kind,
                    }),
                },
                _ => return None,
            },
            ExpressionKind::PlayerScore(_) => self,
            ExpressionKind::Data(_, _) => self,
            ExpressionKind::Unary(UnaryOperator::Reference, expression) => *expression,
            ExpressionKind::Unary(UnaryOperator::Dereference, expression) => {
                expression.dereference(datapack)?.dereference(datapack)?
            }
            _ => return None,
        })
    }

    pub fn dereference_type(
        &self,
        supports_variable_type_scope: &mut impl SupportsVariableTypeScope,
    ) -> Option<DataTypeKind> {
        match &self.kind {
            ExpressionKind::Constant(expression) => Some(
                expression
                    .kind
                    .infer_data_type(supports_variable_type_scope)?
                    .try_dereference(),
            ),
            ExpressionKind::Unary(UnaryOperator::Dereference, expression) => Some(
                expression
                    .dereference_type(supports_variable_type_scope)?
                    .try_dereference(),
            ),
            ExpressionKind::Unary(UnaryOperator::Reference, expression) => expression
                .kind
                .infer_data_type(supports_variable_type_scope),
            _ => None,
        }
    }

    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match &self.kind {
            ExpressionKind::Constant(constant_expression) => {
                constant_expression.perform_semantic_analysis(ctx, is_lhs)
            }
            ExpressionKind::Unary(operator, expression) => {
                let expression_result = expression.perform_semantic_analysis(ctx, is_lhs);

                expression_result?;

                if let Some(data_type) = expression.kind.infer_data_type(ctx) {
                    match operator {
                        UnaryOperator::Negate => {
                            if data_type.get_negated_result().is_none() {
                                return ctx.add_info(SemanticAnalysisInfo {
                                    span: self.span,
                                    kind: SemanticAnalysisInfoKind::Error(
                                        SemanticAnalysisError::CannotNegateType(data_type),
                                    ),
                                });
                            }
                        }
                        UnaryOperator::Reference => {
                            if !expression.kind.is_lvalue() {
                                return ctx.add_info(SemanticAnalysisInfo {
                                    span: expression.span,
                                    kind: SemanticAnalysisInfoKind::Error(
                                        SemanticAnalysisError::CannotBeReferenced(data_type),
                                    ),
                                });
                            }
                        }
                        UnaryOperator::Dereference => {
                            if !expression.kind.can_be_dereferenced() {
                                return ctx.add_info(SemanticAnalysisInfo {
                                    span: expression.span,
                                    kind: SemanticAnalysisInfoKind::Error(
                                        SemanticAnalysisError::CannotBeDereferenced(data_type),
                                    ),
                                });
                            }
                        }
                        UnaryOperator::Invert => {
                            if data_type.get_inverted_result().is_none() {
                                return ctx.add_info(SemanticAnalysisInfo {
                                    span: self.span,
                                    kind: SemanticAnalysisInfoKind::Error(
                                        SemanticAnalysisError::CannotInvertType(data_type),
                                    ),
                                });
                            }
                        }
                    }
                }

                Some(())
            }
            ExpressionKind::Arithmetic(left, operator, right) => {
                let left_result = left.perform_semantic_analysis(ctx, is_lhs);
                let right_result = right.perform_semantic_analysis(ctx, is_lhs);

                left_result?;
                right_result?;

                if let Some(left_type) = left.kind.infer_data_type(ctx)
                    && let Some(right_type) = right.kind.infer_data_type(ctx)
                {
                    let result_type = left_type.get_arithmetic_result(operator, &right_type);

                    if result_type.is_none() {
                        return ctx.add_info(SemanticAnalysisInfo {
                            span: self.span,
                            kind: SemanticAnalysisInfoKind::Error(
                                SemanticAnalysisError::CannotPerformArithmeticOperation {
                                    left: left_type,
                                    operator: *operator,
                                    right: right_type,
                                },
                            ),
                        });
                    }

                    if *operator == ArithmeticOperator::Swap
                        && (!left_type.is_lvalue() || !right_type.is_lvalue())
                    {
                        return ctx.add_info(SemanticAnalysisInfo {
                            span: self.span,
                            kind: SemanticAnalysisInfoKind::Error(
                                SemanticAnalysisError::CannotPerformArithmeticOperation {
                                    left: left_type,
                                    operator: *operator,
                                    right: right_type,
                                },
                            ),
                        });
                    }
                }

                Some(())
            }
            ExpressionKind::Comparison(left, operator, right) => {
                let left_result = left.perform_semantic_analysis(ctx, is_lhs);
                let right_result = right.perform_semantic_analysis(ctx, is_lhs);

                left_result?;
                right_result?;

                if let Some(left_type) = left.kind.infer_data_type(ctx)
                    && let Some(right_type) = right.kind.infer_data_type(ctx)
                    && !left_type.can_perform_comparison(operator, &right_type)
                {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotPerformComparisonOperation {
                                left: left_type,
                                operator: *operator,
                                right: right_type,
                            },
                        ),
                    });
                }

                Some(())
            }
            ExpressionKind::Logical(left, operator, right) => {
                let left_result = left.perform_semantic_analysis(ctx, is_lhs);
                let right_result = right.perform_semantic_analysis(ctx, is_lhs);

                left_result?;
                right_result?;

                if let Some(left_type) = left.kind.infer_data_type(ctx)
                    && let Some(right_type) = right.kind.infer_data_type(ctx)
                    && !left_type.can_perform_logical_comparison(operator, &right_type)
                {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotPerformLogicalOperation {
                                left: left_type,
                                operator: *operator,
                                right: right_type,
                            },
                        ),
                    });
                }

                Some(())
            }
            ExpressionKind::AugmentedAssignment(target, operator, value) => {
                let target_result = target.perform_semantic_analysis(ctx, is_lhs);
                let value_result = value.perform_semantic_analysis(ctx, is_lhs);

                target_result?;
                value_result?;

                let mut error = false;

                let target_type = target.kind.infer_data_type(ctx)?;

                if !target.kind.is_lvalue() {
                    ctx.add_info::<()>(SemanticAnalysisInfo {
                        span: target.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeAssignedTo,
                        ),
                    });

                    error = true;
                }

                let value_type = value.kind.infer_data_type(ctx)?;

                if *operator == ArithmeticOperator::Swap && !value.kind.is_lvalue() {
                    ctx.add_info::<()>(SemanticAnalysisInfo {
                        span: value.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeAssignedTo,
                        ),
                    });

                    error = true;
                }

                if error {
                    return None;
                }

                if !target_type.can_perform_augmented_assignment(operator, &value_type) {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: value.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::InvalidAugmentedAssignmentType(
                                *operator,
                                target_type,
                                value_type,
                            ),
                        ),
                    });
                }

                value_result?;

                Some(())
            }
            ExpressionKind::Assignment(target, value) => {
                let target_result = target.perform_semantic_analysis(ctx, true);
                let value_result = value.perform_semantic_analysis(ctx, false);

                target_result?;

                let Some(place) = target.get_place_type(ctx) else {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: target.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeAssignedTo,
                        ),
                    });
                };

                value_result?;

                let value_data_type = value.kind.infer_data_type(ctx)?;

                place.perform_assignment_semantic_analysis(
                    ctx,
                    *value.clone(),
                    &value_data_type,
                )?;

                Some(())
            }
            ExpressionKind::List(expressions) => expressions
                .iter()
                .map(|expression| expression.perform_semantic_analysis(ctx, is_lhs))
                .all_some(),
            ExpressionKind::Compound(compound) => compound
                .values()
                .map(|value| value.perform_semantic_analysis(ctx, is_lhs))
                .all_some(),
            ExpressionKind::PlayerScore(score) => score.perform_semantic_analysis(ctx, is_lhs),
            ExpressionKind::Data(target, path) => {
                let target_result = target.kind.perform_semantic_analysis(ctx, is_lhs);
                let path_result = path.perform_semantic_analysis(ctx, is_lhs);

                target_result?;
                path_result?;

                Some(())
            }
            ExpressionKind::Condition(_, high_execute_if_subcommand) => {
                high_execute_if_subcommand.perform_semantic_analysis(ctx, is_lhs)
            }
            ExpressionKind::Command(command) => command.perform_semantic_analysis(ctx, is_lhs),
            ExpressionKind::Index(target, index) => {
                let target_result = target.perform_semantic_analysis(ctx, is_lhs);
                let index_result = index.perform_semantic_analysis(ctx, is_lhs);

                target_result?;

                if let Some(target_type) = target.kind.infer_data_type(ctx)
                    && !target_type.can_be_indexed()
                {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: target.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeIndexed(target_type),
                        ),
                    });
                }

                index_result?;

                Some(())
            }
            ExpressionKind::FieldAccess(expression, field) => {
                let expression_result = expression.perform_semantic_analysis(ctx, is_lhs);

                expression_result?;

                if let Some(expression_type) = expression.kind.infer_data_type(ctx) {
                    if !expression_type.has_members() {
                        return ctx.add_info(SemanticAnalysisInfo {
                            span: field.span,
                            kind: SemanticAnalysisInfoKind::Error(
                                SemanticAnalysisError::TypeDoesntHaveFields(expression_type),
                            ),
                        });
                    }

                    if !expression_type.has_member(field) {
                        return ctx.add_info(SemanticAnalysisInfo {
                            span: field.span,
                            kind: SemanticAnalysisInfoKind::Error(
                                SemanticAnalysisError::TypeDoesntHaveField {
                                    data_type: expression_type,
                                    field: field.snbt_string.1.clone(),
                                },
                            ),
                        });
                    }
                }

                Some(())
            }
            ExpressionKind::AsCast(expression, data_type) => {
                let expression_result = expression.perform_semantic_analysis(ctx, is_lhs);
                let data_type_result = data_type.perform_semantic_analysis(ctx, is_lhs);

                expression_result?;
                data_type_result?;

                let expression_type = expression.kind.infer_data_type(ctx)?;
                let data_type = data_type.kind.resolve();

                if !expression_type.can_cast_to(&data_type) {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotCastType {
                                from: expression_type,
                                to: data_type,
                            },
                        ),
                    });
                }

                Some(())
            }
            ExpressionKind::ToCast(expression, runtime_storage) => {
                let expression_result = expression.perform_semantic_analysis(ctx, is_lhs);

                expression_result?;

                if let Some(expression_type) = expression.kind.infer_data_type(ctx) {
                    match runtime_storage {
                        RuntimeStorageType::Score => {
                            if !expression_type.can_be_assigned_to_score() {
                                return ctx.add_info(SemanticAnalysisInfo {
                                    span: expression.span,
                                    kind: SemanticAnalysisInfoKind::Error(
                                        SemanticAnalysisError::CannotBeAssignedToScore(
                                            expression_type,
                                        ),
                                    ),
                                });
                            }
                        }
                        RuntimeStorageType::Data => {
                            if !expression_type.can_be_assigned_to_data() {
                                return ctx.add_info(SemanticAnalysisInfo {
                                    span: expression.span,
                                    kind: SemanticAnalysisInfoKind::Error(
                                        SemanticAnalysisError::CannotBeAssignedToData(
                                            expression_type,
                                        ),
                                    ),
                                });
                            }
                        }
                    }
                }

                Some(())
            }
            ExpressionKind::Tuple(expressions) => expressions
                .iter()
                .map(|expression| expression.perform_semantic_analysis(ctx, is_lhs))
                .all_some(),
        }
    }

    pub fn get_place_type(
        &self,
        supports_variable_type_scope: &mut impl SupportsVariableTypeScope,
    ) -> Option<PlaceType> {
        Some(match &self.kind {
            ExpressionKind::Tuple(expressions) => PlaceType::Tuple(
                expressions
                    .iter()
                    .map(|expression| expression.get_place_type(supports_variable_type_scope))
                    .collect::<Option<_>>()?,
            ),
            ExpressionKind::Constant(expression) => {
                return expression.place_type(supports_variable_type_scope);
            }
            ExpressionKind::PlayerScore(_) => PlaceType::Score,
            ExpressionKind::Data(_, _) => PlaceType::Data,
            ExpressionKind::Unary(UnaryOperator::Dereference, expression) => expression
                .dereference_type(supports_variable_type_scope)?
                .as_place_type()?,
            _ => return None,
        })
    }

    pub fn compile_as_statement(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) {
        match self.kind {
            ExpressionKind::Command(command) => {
                let compiled_command = command.compile(datapack, ctx);

                if let Some(command) = compiled_command {
                    ctx.add_command(datapack, command);
                }
            }
            _ => self
                .resolve(datapack, ctx)
                .kind
                .compile_as_statement(datapack, ctx),
        }
    }

    pub fn assign_to_data(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: DataTarget,
        path: NbtPath,
    ) {
        let resolved = self.resolve(datapack, ctx);

        resolved.kind.assign_to_data(datapack, ctx, target, path);
    }

    pub fn resolve_force(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> ConstantExpression {
        let resolved = self.resolve(datapack, ctx);

        match resolved.kind {
            ConstantExpressionKind::PlayerScore(_) => {
                let unique_score = datapack.get_unique_player_score();

                resolved
                    .kind
                    .assign_to_score(datapack, ctx, unique_score.clone());

                ConstantExpressionKind::PlayerScore(unique_score).into_dummy_constant_expression()
            }
            ConstantExpressionKind::Data(_, _) => {
                let (unique_target, unique_path) = datapack.get_unique_data();

                resolved.kind.assign_to_data(
                    datapack,
                    ctx,
                    unique_target.clone(),
                    unique_path.clone(),
                );

                ConstantExpressionKind::Data(unique_target, unique_path)
                    .into_dummy_constant_expression()
            }
            _ => resolved.resolve(datapack),
        }
    }

    pub fn resolve(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> ConstantExpression {
        match self.kind {
            ExpressionKind::Constant(expression) => expression.resolve(datapack),
            ExpressionKind::Unary(unary_operator, expression) => match unary_operator {
                UnaryOperator::Negate => {
                    let resolved_expression = expression.resolve(datapack, ctx);
                    let new_score = resolved_expression.kind.as_score(datapack, ctx, true);
                    let constant_score = datapack.get_constant_score(-1);

                    ctx.add_command(
                        datapack,
                        Command::Scoreboard(ScoreboardCommand::Players(
                            PlayersScoreboardCommand::Operation(
                                new_score.clone(),
                                ScoreOperationOperator::Multiply,
                                constant_score,
                            ),
                        )),
                    );

                    ConstantExpressionKind::PlayerScore(new_score).into_dummy_constant_expression()
                }
                UnaryOperator::Invert => {
                    let expression = expression.resolve(datapack, ctx);

                    expression
                        .kind
                        .invert()
                        .unwrap()
                        .into_dummy_constant_expression()
                }
                UnaryOperator::Reference => {
                    ConstantExpressionKind::Reference(Box::new(expression.resolve(datapack, ctx)))
                        .into_dummy_constant_expression()
                }
                UnaryOperator::Dereference => expression
                    .resolve(datapack, ctx)
                    .try_dereference(datapack)
                    .unwrap(),
            },
            ExpressionKind::Arithmetic(left, operator, right) => {
                let left = left.resolve(datapack, ctx);
                let right = right.resolve(datapack, ctx);

                (match (left.kind, right.kind) {
                    (
                        ConstantExpressionKind::Literal(left),
                        ConstantExpressionKind::Literal(right),
                    ) => ConstantExpressionKind::Literal(LiteralExpression {
                        span: ParserRange { start: 0, end: 0 },
                        kind: left.kind.perform_arithmetic(operator, right.kind).unwrap(),
                    }),

                    (left_kind @ ConstantExpressionKind::PlayerScore(_), right_kind) => {
                        // TODO maybe better checking?
                        // TODO assign into score

                        let unique_score = datapack.get_unique_player_score();

                        left_kind.assign_to_score(datapack, ctx, unique_score.clone());
                        right_kind.operate_on_score(datapack, ctx, &unique_score, operator);

                        ConstantExpressionKind::PlayerScore(unique_score)
                    }

                    (left_kind, right_kind @ ConstantExpressionKind::PlayerScore(_)) => {
                        // TODO maybe better checking?
                        // TODO assign into score

                        let unique_score = datapack.get_unique_player_score();

                        left_kind.assign_to_score(datapack, ctx, unique_score.clone());
                        right_kind.operate_on_score(datapack, ctx, &unique_score, operator);

                        ConstantExpressionKind::PlayerScore(unique_score)
                    }

                    _ => unreachable!(),
                })
                .into_dummy_constant_expression()
            }
            ExpressionKind::Comparison(left, operator, right) => {
                let left = left.resolve(datapack, ctx);
                let right = right.resolve(datapack, ctx);

                left.kind
                    .compare(datapack, ctx, right.kind, operator)
                    .into_dummy_constant_expression()
            }
            ExpressionKind::Logical(left, operator, right) => {
                let left = left.resolve(datapack, ctx);
                let right = right.resolve(datapack, ctx);

                match operator {
                    LogicalOperator::And => {
                        let unique_score = datapack.get_unique_player_score();

                        let (left_inverted, left_condition) =
                            left.kind.to_execute_condition(datapack, ctx, false);
                        let (right_inverted, right_condition) =
                            right.kind.to_execute_condition(datapack, ctx, false);

                        ctx.add_command(
                            datapack,
                            Command::Scoreboard(ScoreboardCommand::Players(
                                PlayersScoreboardCommand::Set(unique_score.clone(), 0),
                            )),
                        );
                        ConstantExpressionKind::Condition(
                            left_inverted,
                            left_condition.then(ExecuteSubcommand::If(
                                right_inverted,
                                right_condition.then(ExecuteSubcommand::Run(Box::new(
                                    Command::Scoreboard(ScoreboardCommand::Players(
                                        PlayersScoreboardCommand::Set(unique_score.clone(), 1),
                                    )),
                                ))),
                            )),
                        )
                        .into_dummy_constant_expression()
                        .into_constant_expression()
                        .resolve(datapack, ctx)
                        .kind
                        .compile_as_statement(datapack, ctx);

                        ConstantExpressionKind::PlayerScore(unique_score)
                            .into_dummy_constant_expression()
                    }
                    LogicalOperator::Or => {
                        let unique_function_paths = datapack.get_unique_function_paths();

                        let unique_score = datapack.get_unique_player_score();
                        ctx.add_command(
                            datapack,
                            Command::Scoreboard(ScoreboardCommand::Players(
                                PlayersScoreboardCommand::Set(unique_score.clone(), 0),
                            )),
                        );
                        ctx.add_command(
                            datapack,
                            Command::Execute(ExecuteSubcommand::Store(
                                StoreType::Success,
                                ExecuteStoreSubcommand::Score(
                                    unique_score.clone(),
                                    Box::new(ExecuteSubcommand::Run(Box::new(Command::Function(
                                        ResourceLocation::new_namespace_paths(
                                            datapack.current_namespace_name(),
                                            unique_function_paths.clone(),
                                        ),
                                        None,
                                    )))),
                                ),
                            )),
                        );

                        let mut function_ctx = CompileContext::default();

                        let return_one_subcommand = ExecuteSubcommand::Run(Box::new(
                            Command::Return(ReturnCommand::Value(1)),
                        ));

                        let (left_inverted, left_condition) =
                            left.kind
                                .to_execute_condition(datapack, &mut function_ctx, false);
                        function_ctx.add_command(
                            datapack,
                            Command::Execute(ExecuteSubcommand::If(
                                left_inverted,
                                left_condition.then(return_one_subcommand.clone()),
                            )),
                        );
                        let (right_inverted, right_condition) =
                            right
                                .kind
                                .to_execute_condition(datapack, &mut function_ctx, false);
                        function_ctx.add_command(
                            datapack,
                            Command::Execute(ExecuteSubcommand::If(
                                right_inverted,
                                right_condition.then(return_one_subcommand),
                            )),
                        );
                        function_ctx.add_command(datapack, Command::Return(ReturnCommand::Fail));

                        let function_commands = function_ctx.compile();

                        datapack
                            .get_function_mut(&unique_function_paths)
                            .add_commands(function_commands);

                        ConstantExpressionKind::PlayerScore(unique_score)
                            .into_dummy_constant_expression()
                    }
                }
            }
            ExpressionKind::AugmentedAssignment(target, operator, value) => {
                let value = value.resolve(datapack, ctx);

                target
                    .as_place(datapack, ctx)
                    .unwrap()
                    .augmented_assign(datapack, ctx, operator, value.kind);

                ConstantExpressionKind::Unit.into_dummy_constant_expression()
            }
            ExpressionKind::Assignment(target, value) => {
                let value = value.resolve(datapack, ctx);

                target
                    .as_place(datapack, ctx)
                    .unwrap()
                    .assign(datapack, ctx, value);

                ConstantExpressionKind::Unit.into_dummy_constant_expression()
            }
            ExpressionKind::List(expressions) => ConstantExpressionKind::List(
                expressions
                    .into_iter()
                    .map(|expr| expr.resolve(datapack, ctx))
                    .collect::<Vec<_>>(),
            )
            .into_dummy_constant_expression(),
            ExpressionKind::Compound(compound) => ConstantExpressionKind::Compound(
                compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = value.resolve(datapack, ctx);

                        (key, value)
                    })
                    .collect::<BTreeMap<_, _>>(),
            )
            .into_dummy_constant_expression(),
            ExpressionKind::PlayerScore(score) => {
                let score = score.clone().compile(datapack, ctx);

                ConstantExpressionKind::PlayerScore(score).into_dummy_constant_expression()
            }
            ExpressionKind::Data(target, path) => {
                let target = target.kind.clone().compile(datapack, ctx);
                let path = path.clone().compile(datapack, ctx);

                ConstantExpressionKind::Data(target, path).into_dummy_constant_expression()
            }
            ExpressionKind::Condition(inverted, condition) => {
                // TODO why optional?
                let condition = condition.compile(datapack, ctx).unwrap();

                ConstantExpressionKind::Condition(inverted, condition)
                    .into_dummy_constant_expression()
            }
            ExpressionKind::Command(command) => {
                // TODO why optional?
                let command = command.compile(datapack, ctx).unwrap();

                ConstantExpressionKind::Command(command).into_dummy_constant_expression()
            }
            ExpressionKind::Index(target, index) => {
                let target = target.resolve(datapack, ctx);
                let index = index.resolve(datapack, ctx);

                target
                    .kind
                    .index(index.kind)
                    .unwrap()
                    .into_dummy_constant_expression()
            }
            ExpressionKind::FieldAccess(target, member) => {
                let target = target.resolve(datapack, ctx);

                target
                    .kind
                    .access_member(member.snbt_string)
                    .unwrap()
                    .into_dummy_constant_expression()
            }
            ExpressionKind::AsCast(expression, data_type) => {
                let expression = expression.resolve(datapack, ctx);
                let data_type = data_type.kind.resolve();

                expression.map_kind(|kind| kind.cast_to(datapack, data_type).unwrap())
            }
            ExpressionKind::ToCast(expression, runtime_storage_type) => {
                let expression = expression.resolve(datapack, ctx);

                match runtime_storage_type {
                    RuntimeStorageType::Score => {
                        let unique_score = datapack.get_unique_player_score();

                        expression
                            .kind
                            .assign_to_score(datapack, ctx, unique_score.clone());

                        ConstantExpressionKind::PlayerScore(unique_score)
                            .into_dummy_constant_expression()
                    }
                    RuntimeStorageType::Data => {
                        let (unique_target, unique_path) = datapack.get_unique_data();

                        expression.kind.assign_to_data(
                            datapack,
                            ctx,
                            unique_target.clone(),
                            unique_path.clone(),
                        );

                        ConstantExpressionKind::Data(unique_target, unique_path)
                            .into_dummy_constant_expression()
                    }
                }
            }
            ExpressionKind::Tuple(expressions) => ConstantExpressionKind::Tuple(
                expressions
                    .into_iter()
                    .map(|expression| expression.resolve(datapack, ctx))
                    .collect(),
            )
            .into_dummy_constant_expression(),
        }
    }

    pub fn resolve_new_ctx(
        self,
        datapack: &mut HighDatapack,
    ) -> (CompileContext, ConstantExpression) {
        let mut ctx = CompileContext::default();
        let result = self.resolve(datapack, &mut ctx);
        (ctx, result)
    }

    pub fn resolve_into_score(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: &PlayerScore,
    ) {
        match self.kind {
            ExpressionKind::Arithmetic(left, operator, right) => {
                left.resolve_into_score(datapack, ctx, target);
                right
                    .resolve(datapack, ctx)
                    .kind
                    .operate_on_score(datapack, ctx, target, operator);
            }
            _ => {
                self.resolve(datapack, ctx)
                    .kind
                    .assign_to_score(datapack, ctx, target.clone());
            }
        }
    }
}

pub fn compile_shift_operation(
    datapack: &mut HighDatapack,
    ctx: &mut CompileContext,
    target: &PlayerScore,
    amount: i32,
    operator: ScoreOperationOperator,
) {
    if amount <= 0 {
        return;
    }

    let constant_two = datapack.get_constant_score(2);

    for _ in 0..amount {
        ctx.add_command(
            datapack,
            target.clone().operation(operator, constant_two.clone()),
        );
    }
}
