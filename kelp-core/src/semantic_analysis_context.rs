use std::collections::BTreeMap;

use thiserror::Error;

use crate::{
    high::{statement::ControlFlowKind, supports_variable_type_scope::SupportsVariableTypeScope},
    middle::{
        data_type::DataType,
        data_type_declaration::{BuiltinDataType, DataTypeDeclarationKind},
    },
    operator::{ArithmeticOperator, ComparisonOperator},
    pattern_type::PatternType,
    span::Span,
};

const fn format_arithmetic_operator(op: ArithmeticOperator) -> &'static str {
    match op {
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
    }
}

const fn format_control_flow_kind(kind: ControlFlowKind) -> &'static str {
    match kind {
        ControlFlowKind::Break => "break",
        ControlFlowKind::Continue => "continue",
    }
}

#[derive(Debug, Clone, Error)]
pub enum SemanticAnalysisError {
    #[error("Cannot perform: `{}` {} `{}`", left, operator, right)]
    CannotPerformArithmeticOperation {
        left: DataType,
        operator: ArithmeticOperator,
        right: DataType,
    },
    #[error("Cannot perform: `{}` {} `{}`", left, operator, right)]
    CannotPerformComparisonOperation {
        left: DataType,
        operator: ComparisonOperator,
        right: DataType,
    },
    #[error("Cannot perform augmented assignment on type `{}`", .0)]
    CannotPerformAugmentedAssignment(DataType),
    #[error("Expected type `{}` but got `{}`", expected, actual)]
    MismatchedPatternTypes {
        expected: DataType,
        actual: PatternType,
    },
    #[error("The underscore expression can only be used on the left hand side of assignments")]
    UnderscoreExpression,
    #[error("Cannot iterate over type `{}`", .0)]
    CannotIterateType(DataType),
    #[error("Expected type `{}` but got `{}`", expected, actual)]
    MismatchedTypes {
        expected: DataType,
        actual: DataType,
    },
    #[error(
        "Cannot {}-assign type `{}` to type `{}`",
        format_arithmetic_operator(*.0),
        .2,
        .0
    )]
    InvalidAugmentedAssignmentType(ArithmeticOperator, DataType, DataType),
    #[error("Cannot cast type `{}` to `{}`", from, to)]
    CannotCastType { from: DataType, to: DataType },
    #[error("Unknown runtime storage type")]
    UnknownRuntimeStorageType,
    #[error("This value is too big to fit in the type `{}`", .0)]
    ValueTooLarge(DataType),
    #[error("This value is too small to fit in the type `{}`", .0)]
    ValueTooSmall(DataType),
    #[error("Cannot mutate a compile-time value in a runtime loop")]
    CompiletimeValueMutationInRuntimeLoop,
    #[error("The type `{}` is not a struct", .0)]
    TypeIsNotStruct(String),
    #[error("Missing key `{}`", .0)]
    MissingKey(String),
    #[error("Unexpected key `{}`", .0)]
    UnexpectedKey(String),
    #[error("Missing field `{}`", .0)]
    MissingField(String),
    #[error("Unexpected field `{}`", .0)]
    UnexpectedField(String),
    #[error("The type `{}` has already been declared in this scope", .0)]
    TypeIsAlreadyDefined(String),
    #[error("This pattern is not irrefutable")]
    PatternIsNotIrrefutable,
    #[error("Unknown type `{}`", .0)]
    UnknownType(String),
    #[error("The type `{}` cannot be used in conditions", .0)]
    TypeIsNotCondition(DataType),
    #[error("The type `{}` is not score compatible", .0)]
    TypeIsNotScoreCompatible(DataType),
    #[error("The type `{}` cannot be assigned to data storage", .0)]
    CannotBeAssignedToData(DataType),
    #[error("The type `{}` cannot be indexed", .0)]
    CannotBeIndexed(DataType),
    #[error("Index out of bounds")]
    IndexOutOfBounds,
    #[error("The type `{}` cannot be dereferenced", .0)]
    CannotBeDereferenced(DataType),
    #[error("The type `{}` cannot be referenced", .0)]
    CannotBeReferenced(DataType),
    #[error("Cannot assign a value to this expression")]
    CannotBeAssignedTo,
    #[error("The type `{}` does not have any fields", .0)]
    TypeDoesntHaveFields(DataType),
    #[error("The type `{}` cannot be negated", .0)]
    CannotNegateType(DataType),
    #[error("The type `{}` cannot be inverted", .0)]
    CannotInvertType(DataType),
    #[error("The type `{}` does not have a field named `{}`", data_type, field)]
    TypeDoesntHaveField { data_type: DataType, field: String },
    #[error(
        "The type `{}` takes {} generic argument{} but {} {} given",
        data_type_kind,
        expected,
        if *.expected == 1 { "" } else { "s" },
        actual,
        if *.actual == 1 { "was" } else { "were" }
    )]
    InvalidGenerics {
        data_type_kind: String,
        expected: usize,
        actual: usize,
    },
    #[error("The variable `{}` has not been declared in the current scope", .0)]
    UndeclaredVariable(String),
    #[error("This string conflicts with the compiler-generated command macros")]
    MacroConflict,
    #[error("Cannot `{}` outside of a loop", format_control_flow_kind(*.0))]
    ControlFlowNotInLoop(ControlFlowKind),
}

#[derive(Debug, Clone)]
pub enum SemanticAnalysisInfoKind {
    // Warning(SemanticAnalysisWarning),
    Error(SemanticAnalysisError),
}

#[derive(Debug, Clone)]
pub struct SemanticAnalysisInfo {
    pub span: Span,
    pub kind: SemanticAnalysisInfoKind,
}

#[derive(Debug, Default, Clone)]
pub struct Scope {
    pub variables: BTreeMap<String, Option<DataType>>,
    pub data_types: BTreeMap<String, Option<DataTypeDeclarationKind>>,
}

impl Scope {
    pub fn declare_variable(&mut self, name: String, value: Option<DataType>) {
        self.variables.insert(name, value);
    }

    #[inline]
    #[must_use]
    pub fn variable_is_declared(&self, name: &str) -> bool {
        self.variables.contains_key(name)
    }

    #[inline]
    #[must_use]
    pub fn get_variable(&self, name: &str) -> Option<&Option<DataType>> {
        self.variables.get(name)
    }

    pub fn declare_data_type(&mut self, name: &str, kind: Option<DataTypeDeclarationKind>) {
        self.data_types.insert(name.to_owned(), kind);
    }

    #[inline]
    #[must_use]
    pub fn data_type_is_declared(&self, name: &str) -> bool {
        self.data_types.contains_key(name)
    }

    #[inline]
    #[must_use]
    pub fn get_data_type(&self, name: &str) -> Option<&Option<DataTypeDeclarationKind>> {
        self.data_types.get(name)
    }
}

pub type Scopes = Vec<Scope>;

#[derive(Debug, Clone)]
pub struct SemanticAnalysisContext {
    pub infos: Vec<SemanticAnalysisInfo>,
    pub max_infos: usize,
    pub scopes: Scopes,
    pub loop_depth: u32,
    pub is_lhs: bool,
}

impl SemanticAnalysisContext {
    #[must_use]
    pub fn new(max_infos: usize) -> Self {
        let mut scopes = Vec::new();

        let mut scope = Scope::default();

        scope.declare_data_type(
            "boolean",
            Some(DataTypeDeclarationKind::Builtin(BuiltinDataType::Boolean)),
        );
        scope.declare_data_type(
            "byte",
            Some(DataTypeDeclarationKind::Builtin(BuiltinDataType::Byte)),
        );
        scope.declare_data_type(
            "short",
            Some(DataTypeDeclarationKind::Builtin(BuiltinDataType::Short)),
        );
        scope.declare_data_type(
            "integer",
            Some(DataTypeDeclarationKind::Builtin(BuiltinDataType::Integer)),
        );
        scope.declare_data_type(
            "long",
            Some(DataTypeDeclarationKind::Builtin(BuiltinDataType::Long)),
        );
        scope.declare_data_type(
            "float",
            Some(DataTypeDeclarationKind::Builtin(BuiltinDataType::Float)),
        );
        scope.declare_data_type(
            "double",
            Some(DataTypeDeclarationKind::Builtin(BuiltinDataType::Double)),
        );
        scope.declare_data_type(
            "string",
            Some(DataTypeDeclarationKind::Builtin(BuiltinDataType::String)),
        );
        // scope.declare_data_type(
        //     "unit",
        //     Some(DataTypeDeclarationKind::Builtin(BuiltinDataType::Unit)),
        // );
        scope.declare_data_type(
            "score",
            Some(DataTypeDeclarationKind::Builtin(BuiltinDataType::Score)),
        );
        scope.declare_data_type(
            "list",
            Some(DataTypeDeclarationKind::Builtin(BuiltinDataType::List)),
        );
        scope.declare_data_type(
            "compound",
            Some(DataTypeDeclarationKind::Builtin(BuiltinDataType::Compound)),
        );
        scope.declare_data_type(
            "data",
            Some(DataTypeDeclarationKind::Builtin(BuiltinDataType::Data)),
        );
        scope.declare_data_type(
            "snbt",
            Some(DataTypeDeclarationKind::Builtin(BuiltinDataType::SNBT)),
        );

        scopes.push(scope);

        Self {
            infos: Vec::new(),
            max_infos,
            scopes,
            loop_depth: 0,
            is_lhs: false,
        }
    }

    #[inline]
    pub fn add_invalid_generics<T>(
        &mut self,
        span: Span,
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

    pub fn add_info<T>(&mut self, info: SemanticAnalysisInfo) -> Option<T> {
        if self.infos.len() >= self.max_infos {
            return None;
        }

        self.infos.push(info);

        None
    }

    pub fn add_error(&mut self, span: Span, error: SemanticAnalysisError) {
        self.add_error_ret::<()>(span, error);
    }

    pub fn add_error_ret<T>(&mut self, span: Span, error: SemanticAnalysisError) -> Option<T> {
        self.add_info(SemanticAnalysisInfo {
            span,
            kind: SemanticAnalysisInfoKind::Error(error),
        })
    }

    pub fn declare_variable(&mut self, name: &str, data_type: Option<DataType>) {
        self.scopes
            .last_mut()
            .expect("No scopes")
            .declare_variable(name.to_string(), data_type);
    }

    #[inline]
    pub fn declare_variable_known(&mut self, name: &str, data_type: DataType) {
        self.declare_variable(name, Some(data_type));
    }

    #[inline]
    pub fn declare_variable_unknown(&mut self, name: &str) {
        self.declare_variable(name, None);
    }

    pub fn declare_data_type(&mut self, name: &str, kind: Option<DataTypeDeclarationKind>) {
        self.scopes
            .last_mut()
            .expect("No scopes")
            .declare_data_type(name, kind);
    }

    #[must_use]
    pub fn variable_is_declared(&self, name: &str) -> bool {
        self.scopes
            .last()
            .is_some_and(|scope| scope.variable_is_declared(name))
    }

    #[must_use]
    pub fn data_type_is_declared(&self, name: &str) -> bool {
        self.scopes
            .last()
            .is_some_and(|scope| scope.data_type_is_declared(name))
    }

    #[must_use]
    pub fn get_variable(&self, name: &str) -> Option<Option<DataType>> {
        for scope in self.scopes.iter().rev() {
            if let Some(data_type) = scope.get_variable(name) {
                return Some(data_type.clone());
            }
        }

        None
    }

    #[must_use]
    pub fn get_data_type(&self, name: &str) -> Option<Option<DataTypeDeclarationKind>> {
        for scope in self.scopes.iter().rev() {
            if let Some(data_type) = scope.get_data_type(name) {
                return Some(data_type.clone());
            }
        }

        None
    }
}

impl SupportsVariableTypeScope for SemanticAnalysisContext {
    fn get_variable(&self, name: &str) -> Option<Option<DataType>> {
        self.get_variable(name)
    }

    fn get_data_type(&self, name: &str) -> Option<Option<DataTypeDeclarationKind>> {
        self.get_data_type(name)
    }
}
