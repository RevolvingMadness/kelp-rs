use std::fmt::{Display, Write};

use minecraft_command_types::command::{
    enums::score_operation_operator::ScoreOperationOperator, execute::ScoreComparisonOperator,
};

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
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
        })
    }
}

impl TryFrom<ArithmeticOperator> for ScoreOperationOperator {
    type Error = ();

    fn try_from(value: ArithmeticOperator) -> Result<Self, Self::Error> {
        Ok(match value {
            ArithmeticOperator::Add => Self::Add,
            ArithmeticOperator::Subtract => Self::Subtract,
            ArithmeticOperator::Multiply => Self::Multiply,
            ArithmeticOperator::FloorDivide => Self::Divide,
            ArithmeticOperator::Modulo => Self::Modulo,
            _ => return Err(()),
        })
    }
}

impl ArithmeticOperator {
    #[must_use]
    pub const fn name(&self) -> &str {
        match self {
            Self::Add => "add",
            Self::Subtract => "subtract",
            Self::Multiply => "multiply",
            Self::FloorDivide => "divide",
            Self::Modulo => "modulo",
            Self::And => "and",
            Self::Or => "or",
            Self::LeftShift => "left-shift",
            Self::RightShift => "right-shift",
        }
    }

    #[must_use]
    pub const fn is_additive(self) -> bool {
        matches!(self, Self::Add | Self::Subtract)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
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

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
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

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum UnaryOperator {
    Negate,
    Reference,
    Dereference,
    Invert,
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let symbol = match self {
            Self::Negate => '-',
            Self::Reference => '&',
            Self::Dereference => '*',
            Self::Invert => '!',
        };

        f.write_char(symbol)
    }
}
