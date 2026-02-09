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
