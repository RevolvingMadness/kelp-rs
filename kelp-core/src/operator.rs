use std::fmt::Display;

use minecraft_command_types::command::{
    enums::score_operation_operator::ScoreOperationOperator, execute::ScoreComparisonOperator,
};
use minecraft_command_types_derive::HasMacro;

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
            Self::Add => "+",
            Self::Subtract => "-",
            Self::Multiply => "*",
            Self::FloorDivide => "/",
            Self::Modulo => "%",
            Self::And => "&",
            Self::Or => "|",
            Self::LeftShift => "<<",
            Self::RightShift => ">>",
            Self::Swap => "><",
        })
    }
}

impl ArithmeticOperator {
    #[must_use]
    pub const fn is_additive(self) -> bool {
        matches!(self, Self::Add | Self::Subtract)
    }

    #[must_use]
    pub const fn into_scoreboard_players_operation_operator(
        self,
    ) -> Option<ScoreOperationOperator> {
        match self {
            Self::Add => Some(ScoreOperationOperator::Add),
            Self::Subtract => Some(ScoreOperationOperator::Subtract),
            Self::Multiply => Some(ScoreOperationOperator::Multiply),
            Self::FloorDivide => Some(ScoreOperationOperator::Divide),
            Self::Modulo => Some(ScoreOperationOperator::Modulo),
            Self::Swap => Some(ScoreOperationOperator::Swap),
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
            Self::And => "&&",
            Self::Or => "||",
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
            Self::LessThan => "<",
            Self::LessThanOrEqualTo => "<=",
            Self::GreaterThan => ">",
            Self::GreaterThanOrEqualTo => ">=",
            Self::EqualTo => "==",
            Self::NotEqualTo => "!=",
        })
    }
}

impl ComparisonOperator {
    #[must_use]
    pub const fn into_score_comparison_operator(self) -> ScoreComparisonOperator {
        match self {
            Self::LessThan => ScoreComparisonOperator::LessThan,
            Self::LessThanOrEqualTo => ScoreComparisonOperator::LessThanOrEqualTo,
            Self::GreaterThan => ScoreComparisonOperator::GreaterThan,
            Self::GreaterThanOrEqualTo => ScoreComparisonOperator::GreaterThanOrEqualTo,
            Self::EqualTo | Self::NotEqualTo => ScoreComparisonOperator::EqualTo,
        }
    }

    #[must_use]
    pub const fn should_execute_if_be_inverted(&self) -> bool {
        matches!(self, Self::NotEqualTo)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum UnaryOperator {
    Negate,
    Reference,
    Dereference,
    Invert,
}
