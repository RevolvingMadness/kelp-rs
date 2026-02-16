use std::{
    collections::{BTreeMap, VecDeque},
    fmt::Display,
};

use parser_rs::parser_range::ParserRange;

use crate::{
    data_type::DataTypeKind,
    datapack::DataTypeDeclarationKind,
    expression::supports_variable_type_scope::SupportsVariableTypeScope,
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator},
    pattern_type::PatternType,
};

#[derive(Debug, Clone)]
pub enum SemanticAnalysisError {
    CannotPerformArithmeticOperation {
        left: DataTypeKind,
        operator: ArithmeticOperator,
        right: DataTypeKind,
    },
    CannotPerformComparisonOperation {
        left: DataTypeKind,
        operator: ComparisonOperator,
        right: DataTypeKind,
    },
    CannotPerformLogicalOperation {
        left: DataTypeKind,
        operator: LogicalOperator,
        right: DataTypeKind,
    },
    CannotPerformAugmentedAssignment(DataTypeKind),
    MismatchedPatternTypes {
        expected: DataTypeKind,
        actual: PatternType,
    },
    UnderscoreExpression,
    CannotIterateType(DataTypeKind),
    MismatchedTypes {
        expected: DataTypeKind,
        actual: DataTypeKind,
    },
    InvalidAugmentedAssignmentType(ArithmeticOperator, DataTypeKind, DataTypeKind),
    CannotCastType {
        from: DataTypeKind,
        to: DataTypeKind,
    },
    TypeIsNotStruct(String),
    MissingKey(String),
    UnexpectedKey(String),
    MissingField(String),
    UnexpectedField(String),
    TypeIsAlreadyDefined(String),
    PatternIsNotIrrefutable,
    UnknownType(String),
    TypeIsNotCondition(DataTypeKind),
    CannotBeAssignedToScore(DataTypeKind),
    CannotBeAssignedToData(DataTypeKind),
    CannotBeIndexed(DataTypeKind),
    CannotBeDereferenced(DataTypeKind),
    CannotBeReferenced(DataTypeKind),
    CannotBeAssignedTo,
    TypeDoesntHaveFields(DataTypeKind),
    CannotNegateType(DataTypeKind),
    CannotInvertType(DataTypeKind),
    TypeDoesntHaveField {
        data_type: DataTypeKind,
        field: String,
    },
    InvalidGenerics {
        data_type_kind: String,
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
            Self::CannotPerformAugmentedAssignment(data_type) => write!(
                f,
                "Cannot perform augmented assignment on type '{}'",
                data_type
            ),
            Self::MismatchedTypes { expected, actual } => {
                write!(f, "Expected type '{}' but got '{}'", expected, actual)
            }
            Self::MismatchedPatternTypes { expected, actual } => {
                write!(f, "Expected type '{}' but got '{}'", expected, actual)
            }
            Self::UnderscoreExpression => f.write_str(
                "The underscore expression can only be used on the left hand side of assignments",
            ),
            Self::PatternIsNotIrrefutable => f.write_str("This pattern is not irrefutable"),
            Self::UnknownType(name) => {
                write!(f, "Unknown type '{}'", name)
            }
            Self::TypeIsNotStruct(name) => {
                write!(f, "The type '{}' is not a struct", name)
            }
            Self::CannotIterateType(data_type) => {
                write!(f, "Cannot iterate over type '{}'", data_type)
            }
            Self::MissingKey(key) => {
                write!(f, "Missing key '{}'", key)
            }
            Self::UnexpectedKey(key) => {
                write!(f, "Unexpected key '{}'", key)
            }
            Self::MissingField(field) => {
                write!(f, "Missing field '{}'", field)
            }
            Self::UnexpectedField(field) => {
                write!(f, "Unexpected field '{}'", field)
            }
            Self::TypeIsAlreadyDefined(name) => {
                write!(
                    f,
                    "The type '{}' has already been declared in this scope",
                    name
                )
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
            Self::CannotBeAssignedTo => f.write_str("Cannot assign a value to this expression"),
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
                    "The type '{}' does not have a field named '{}'",
                    data_type, field
                )
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

#[derive(Debug, Default, Clone)]
pub struct Scope {
    pub variables: BTreeMap<String, Option<DataTypeKind>>,
    pub variable_types: BTreeMap<String, Option<DataTypeKind>>,
    pub types: BTreeMap<String, Option<DataTypeDeclarationKind>>,
}

impl Scope {
    pub fn declare_variable(&mut self, name: String, value: Option<DataTypeKind>) {
        self.variable_types.insert(name, value);
    }

    pub fn variable_is_declared(&self, name: &str) -> bool {
        self.variable_types.contains_key(name)
    }

    pub fn get_variable(&self, name: &str) -> Option<&Option<DataTypeKind>> {
        self.variable_types.get(name)
    }

    pub fn declare_data_type(&mut self, name: String, kind: Option<DataTypeDeclarationKind>) {
        self.types.insert(name, kind);
    }

    pub fn data_type_is_declared(&self, name: &str) -> bool {
        self.types.contains_key(name)
    }

    pub fn get_data_type(&self, name: &str) -> Option<&Option<DataTypeDeclarationKind>> {
        self.types.get(name)
    }
}

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
        data_type: String,
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

    pub fn declare_variable(&mut self, name: &str, data_type: Option<DataTypeKind>) {
        self.scopes
            .front_mut()
            .expect("No scopes")
            .declare_variable(name.to_string(), data_type);
    }

    #[inline]
    pub fn declare_variable_known(&mut self, name: &str, data_type: DataTypeKind) {
        self.declare_variable(name, Some(data_type));
    }

    #[inline]
    pub fn declare_variable_unknown(&mut self, name: &str) {
        self.declare_variable(name, None);
    }

    pub fn declare_data_type(&mut self, name: String, kind: Option<DataTypeDeclarationKind>) {
        self.scopes
            .front_mut()
            .expect("No scopes")
            .declare_data_type(name, kind);
    }

    pub fn variable_is_declared(&self, name: &str) -> bool {
        self.scopes
            .iter()
            .rev()
            .any(|scope| scope.variable_is_declared(name))
    }

    pub fn data_type_is_declared(&self, name: &str) -> bool {
        self.scopes
            .iter()
            .rev()
            .any(|scope| scope.data_type_is_declared(name))
    }

    pub fn get_variable(&self, name: &str) -> Option<Option<DataTypeKind>> {
        for scope in &self.scopes {
            if let Some(data_type) = scope.get_variable(name) {
                return Some(data_type.clone());
            }
        }

        None
    }

    pub fn get_data_type(&self, name: &str) -> Option<Option<DataTypeDeclarationKind>> {
        for scope in &self.scopes {
            if let Some(data_type) = scope.get_data_type(name) {
                return Some(data_type.clone());
            }
        }

        match name {
            "boolean" | "byte" | "short" | "integer" | "int" | "long" | "float" | "double"
            | "string" | "str" | "score" | "list" | "compound" | "data" | "snbt" => {
                Some(Some(DataTypeDeclarationKind::Builtin(name.to_string())))
            }
            _ => None,
        }
    }
}

impl SupportsVariableTypeScope for SemanticAnalysisContext {
    fn get_variable(&self, name: &str) -> Option<Option<DataTypeKind>> {
        self.get_variable(name)
    }

    fn add_info(&mut self, semantic_analysis_info: SemanticAnalysisInfo) {
        self.add_info::<()>(semantic_analysis_info);
    }

    fn get_data_type(&self, name: &str) -> Option<Option<DataTypeDeclarationKind>> {
        self.get_data_type(name)
    }
}
