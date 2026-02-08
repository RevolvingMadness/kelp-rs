use std::{
    collections::{BTreeMap, VecDeque},
    fmt::Display,
};

use parser_rs::parser_range::ParserRange;

use crate::{
    data_type::{DataType, DataTypeKind},
    expression::{
        ArithmeticOperator, ComparisonOperator, LogicalOperator, SupportsVariableTypeScope,
    },
};

#[derive(Debug, Clone)]
pub enum SemanticAnalysisError {
    CannotPerformArithmeticOperation {
        left: DataType,
        operator: ArithmeticOperator,
        right: DataType,
    },
    CannotPerformComparisonOperation {
        left: DataType,
        operator: ComparisonOperator,
        right: DataType,
    },
    CannotPerformLogicalOperation {
        left: DataType,
        operator: LogicalOperator,
        right: DataType,
    },
    MismatchedTypes {
        expected: DataType,
        actual: DataType,
    },
    InvalidAugmentedAssignmentType(ArithmeticOperator, DataType, DataType),
    CannotCastType {
        from: DataType,
        to: DataType,
    },
    TypeIsNotCondition(DataType),
    CannotBeAssignedToScore(DataType),
    CannotBeAssignedToData(DataType),
    CannotBeIndexed(DataType),
    CannotBeDereferenced(DataType),
    CannotBeReferenced(DataType),
    CannotBeAssignedTo(DataType),
    TypeDoesntHaveFields(DataType),
    CannotNegateType(DataType),
    CannotInvertType(DataType),
    TypeRequiresReference(DataType),
    TypeDoesntHaveField {
        data_type: DataType,
        field: String,
    },
    InvalidGenerics {
        data_type_kind: DataTypeKind,
        expected: usize,
        actual: usize,
    },
    UndeclaredVariable(String),
    MacroConflict,
}

impl Display for SemanticAnalysisError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CannotPerformArithmeticOperation {
                left,
                operator,
                right,
            } => write!(f, "Cannot perform: {} {} {}", left, operator, right),
            Self::CannotPerformComparisonOperation {
                left,
                operator,
                right,
            } => write!(f, "Cannot perform: {} {} {}", left, operator, right),
            Self::CannotPerformLogicalOperation {
                left,
                operator,
                right,
            } => write!(f, "Cannot perform: {} {} {}", left, operator, right),
            Self::MismatchedTypes { expected, actual } => {
                write!(f, "Expected type '{}' but got '{}'", expected, actual)
            }
            Self::InvalidAugmentedAssignmentType(operator, target_type, value_type) => {
                let word = match operator {
                    ArithmeticOperator::Add => "add",
                    ArithmeticOperator::Subtract => "subtract",
                    ArithmeticOperator::Multiply => "multiply",
                    ArithmeticOperator::FloorDivide => "divide",
                    ArithmeticOperator::Modulo => "modulo",
                    ArithmeticOperator::And => "and",
                    ArithmeticOperator::Or => "or",
                    ArithmeticOperator::LeftShift => "left-shift",
                    ArithmeticOperator::RightShift => "right-shift",
                    ArithmeticOperator::Swap => "swap",
                };

                write!(
                    f,
                    "Cannot {}-assign type '{}' to type '{}'",
                    word, value_type, target_type
                )
            }
            Self::CannotCastType { from, to } => {
                write!(f, "Cannot cast type '{}' to '{}'", from, to)
            }
            Self::TypeIsNotCondition(data_type) => {
                write!(f, "The type '{}' cannot be used in conditions", data_type)
            }
            Self::CannotBeAssignedToScore(data_type) => {
                write!(f, "The type '{}' cannot be assigned to a score", data_type)
            }
            Self::CannotBeAssignedToData(data_type) => {
                write!(
                    f,
                    "The type '{}' cannot be assigned to data storage",
                    data_type
                )
            }
            Self::CannotBeIndexed(data_type) => {
                write!(f, "The type '{}' cannot be indexed", data_type)
            }
            Self::CannotBeDereferenced(data_type) => {
                write!(f, "The type '{}' cannot be dereferenced", data_type)
            }
            Self::CannotBeReferenced(data_type) => {
                write!(f, "The type '{}' cannot be referenced", data_type)
            }
            Self::CannotBeAssignedTo(data_type) => {
                write!(f, "The type '{}' cannot be assigned a value", data_type)
            }
            Self::TypeDoesntHaveFields(data_type) => {
                write!(f, "The type '{}' does not have any fields", data_type)
            }
            Self::CannotNegateType(data_type) => {
                write!(f, " The type '{}' cannot be negated", data_type)
            }
            Self::CannotInvertType(data_type) => {
                write!(f, " The type '{}' cannot be inverted", data_type)
            }
            Self::TypeDoesntHaveField { data_type, field } => {
                write!(
                    f,
                    "The type '{}' doesn't have the field '{}'",
                    data_type, field
                )
            }
            Self::TypeRequiresReference(data_type) => {
                write!(f, "The type '{}' requires a reference", data_type)
            }
            Self::InvalidGenerics {
                data_type_kind: data_type,
                expected,
                actual,
            } => {
                write!(
                    f,
                    "The type '{}' takes {} generic argument",
                    data_type, expected
                )?;

                if *expected != 1 {
                    f.write_str("s")?;
                }

                write!(f, " but {} ", actual)?;

                if *actual == 1 {
                    f.write_str("was")?;
                } else {
                    f.write_str("were")?;
                }

                f.write_str(" given")
            }
            Self::UndeclaredVariable(name) => {
                write!(
                    f,
                    "The variable '{}' has not been declared in the current scope",
                    name
                )
            }
            Self::MacroConflict => {
                f.write_str("This string conflicts with the compiler-generated command macros")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum SemanticAnalysisInfoKind {
    // Warning(SemanticAnalysisWarning),
    Error(SemanticAnalysisError),
}

#[derive(Debug, Clone)]
pub struct SemanticAnalysisInfo {
    pub span: ParserRange,
    pub kind: SemanticAnalysisInfoKind,
}

pub type Scope = BTreeMap<String, Option<DataType>>;
pub type Scopes = VecDeque<Scope>;

#[derive(Debug, Default, Clone)]
pub struct SemanticAnalysisContext {
    pub infos: Vec<SemanticAnalysisInfo>,
    pub max_infos: usize,
    pub scopes: Scopes,
}

impl SemanticAnalysisContext {
    #[inline]
    pub fn add_invalid_generics<T>(
        &mut self,
        span: ParserRange,
        data_type: DataTypeKind,
        expected: usize,
        actual: usize,
    ) -> Option<T> {
        self.add_info(SemanticAnalysisInfo {
            span,
            kind: SemanticAnalysisInfoKind::Error(SemanticAnalysisError::InvalidGenerics {
                data_type_kind: data_type,
                expected,
                actual,
            }),
        })
    }

    #[inline]
    pub fn add_info<T>(&mut self, info: SemanticAnalysisInfo) -> Option<T> {
        if self.infos.len() >= self.max_infos {
            return None;
        }

        self.infos.push(info);

        None
    }

    pub fn declare_variable(&mut self, name: &str, data_type: Option<DataType>) {
        self.scopes
            .front_mut()
            .expect("No scopes")
            .insert(name.to_string(), data_type);
    }

    #[inline]
    pub fn declare_variable_known(&mut self, name: &str, data_type: DataType) {
        self.declare_variable(name, Some(data_type));
    }

    #[inline]
    pub fn declare_variable_unknown<T>(&mut self, name: &str) -> Option<T> {
        self.declare_variable(name, None);

        None
    }

    pub fn variable_is_declared(&self, name: &str) -> bool {
        self.scopes
            .iter()
            .rev()
            .any(|scope| scope.contains_key(name))
    }

    pub fn get_variable(&self, name: &str) -> Option<Option<DataType>> {
        for scope in &self.scopes {
            if let Some(value) = scope.get(name) {
                return Some(value.clone());
            }
        }

        None
    }
}

impl SupportsVariableTypeScope for SemanticAnalysisContext {
    fn get_variable(&self, name: &str) -> Option<Option<DataType>> {
        self.get_variable(name)
    }

    fn add_info(&mut self, semantic_analysis_info: SemanticAnalysisInfo) {
        self.add_info::<()>(semantic_analysis_info);
    }
}
