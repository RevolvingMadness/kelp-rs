use std::fmt::Display;

use crate::{
    typed::{data_type::unresolved::UnresolvedDataType, statement::LoopControlFlowKind},
    operator::{ArithmeticOperator, ComparisonOperator},
    parsed::environment::resolved::ResolvedEnvironment,
    pattern_type::PatternType,
};

pub struct SemanticAnalysisErrorDisplay<'a> {
    pub error: &'a SemanticAnalysisError,
    pub resolved_environment: &'a ResolvedEnvironment,
}

impl Display for SemanticAnalysisErrorDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.error {
            SemanticAnalysisError::InherentImplRequiresNomialType => {
                f.write_str("An inherent `impl` requires a nomial type")
            }
            SemanticAnalysisError::FunctionTypesNotAllRuntime => {
                f.write_str("Every type in a `runtime` function signature must be runtime")
            }
            SemanticAnalysisError::FunctionTypesNotAllData => {
                f.write_str("Every type in a `recursive` function signature must be `data<_>`")
            }
            SemanticAnalysisError::RecursiveFunctionNotRuntime => {
                f.write_str("A function marked as `recursive` must also be marked as `runtime`")
            }
            SemanticAnalysisError::CannotPerformArithmeticOperation {
                left,
                operator,
                right,
            } => {
                write!(
                    f,
                    "Cannot perform: `{}` {} `{}`",
                    left.display(self.resolved_environment),
                    operator,
                    right.display(self.resolved_environment)
                )
            }
            SemanticAnalysisError::CannotPerformComparisonOperation {
                left,
                operator,
                right,
            } => {
                write!(
                    f,
                    "Cannot perform: `{}` {} `{}`",
                    left.display(self.resolved_environment),
                    operator,
                    right.display(self.resolved_environment)
                )
            }
            SemanticAnalysisError::CannotPerformAugmentedAssignment(data_type) => {
                write!(
                    f,
                    "Cannot perform augmented assignment on type `{}`",
                    data_type.display(self.resolved_environment)
                )
            }
            SemanticAnalysisError::MismatchedPatternTypes { expected, actual } => {
                write!(
                    f,
                    "Expected type `{}` but got `{}`",
                    expected.display(self.resolved_environment),
                    actual
                )
            }
            SemanticAnalysisError::UnderscoreExpression => write!(
                f,
                "The underscore expression can only be used as an assignee"
            ),
            SemanticAnalysisError::CannotIterateType(data_type) => {
                write!(
                    f,
                    "Cannot iterate over type `{}`",
                    data_type.display(self.resolved_environment)
                )
            }
            SemanticAnalysisError::MismatchedTypes { expected, actual } => {
                write!(
                    f,
                    "Expected type `{}` but got `{}`",
                    expected.display(self.resolved_environment),
                    actual.display(self.resolved_environment)
                )
            }
            SemanticAnalysisError::InvalidAugmentedAssignmentType(op, target, value) => {
                write!(
                    f,
                    "Cannot {}-assign type `{}` to type `{}`",
                    op.name(),
                    value.display(self.resolved_environment),
                    target.display(self.resolved_environment)
                )
            }
            SemanticAnalysisError::CannotCastType { from, to } => {
                write!(
                    f,
                    "Cannot cast type `{}` to `{}`",
                    from.display(self.resolved_environment),
                    to.display(self.resolved_environment)
                )
            }
            SemanticAnalysisError::CannotUseReturnInCompiletimeFunction => {
                write!(
                    f,
                    "`return` can only be used in functions marked as `runtime`"
                )
            }
            SemanticAnalysisError::CannotBeRepresentedAsFloat(data_type) => {
                write!(
                    f,
                    "The type `{}` cannot be represented as a float",
                    data_type.display(self.resolved_environment)
                )
            }
            SemanticAnalysisError::RecursiveFunctionCall => f.write_str(
                "Recursive function calls can only be used inside functions marked as `recursive`",
            ),
            SemanticAnalysisError::CompiletimeValueMutationInRuntimeLoop => {
                f.write_str("Cannot mutate a compile-time value in a runtime loop")
            }
            SemanticAnalysisError::MissingKey(key) => write!(f, "Missing key `{}`", key),
            SemanticAnalysisError::UnexpectedKey(key) => write!(f, "Unexpected key `{}`", key),
            SemanticAnalysisError::MissingField(field) => {
                write!(f, "Missing field `{}`", field)
            }
            SemanticAnalysisError::TypeAlreadyDeclared(name) => {
                write!(
                    f,
                    "A type with the name `{}` has already been declared",
                    name
                )
            }
            SemanticAnalysisError::ValueAlreadyDeclared(name) => {
                write!(
                    f,
                    "A value with the name `{}` has already been declared",
                    name
                )
            }
            SemanticAnalysisError::PatternIsNotIrrefutable => {
                write!(f, "This pattern is not irrefutable")
            }
            SemanticAnalysisError::TypeIsNotCondition(data_type) => {
                write!(
                    f,
                    "The type `{}` cannot be used in conditions",
                    data_type.display(self.resolved_environment)
                )
            }
            SemanticAnalysisError::TypeIsNotScoreCompatible(data_type) => {
                write!(
                    f,
                    "The type `{}` is not score compatible",
                    data_type.display(self.resolved_environment)
                )
            }
            SemanticAnalysisError::TypeIsNotDataCompatible(data_type) => {
                write!(
                    f,
                    "The type `{}` is not data compatible",
                    data_type.display(self.resolved_environment)
                )
            }
            SemanticAnalysisError::CannotBeAssignedToData(data_type) => {
                write!(
                    f,
                    "The type `{}` cannot be assigned to data storage",
                    data_type.display(self.resolved_environment)
                )
            }
            SemanticAnalysisError::CannotBeIndexed(data_type) => {
                write!(
                    f,
                    "The type `{}` cannot be indexed",
                    data_type.display(self.resolved_environment)
                )
            }
            SemanticAnalysisError::CannotBeIndexedByType { target, index } => {
                write!(
                    f,
                    "The type `{}` cannot be indexed by `{}`",
                    target.display(self.resolved_environment),
                    index.display(self.resolved_environment),
                )
            }
            SemanticAnalysisError::IndexOutOfBounds => write!(f, "Index out of bounds"),
            SemanticAnalysisError::CannotBeDereferenced(data_type) => {
                write!(
                    f,
                    "The type `{}` cannot be dereferenced",
                    data_type.display(self.resolved_environment)
                )
            }
            SemanticAnalysisError::CannotBeReferenced(data_type) => {
                write!(
                    f,
                    "The type `{}` cannot be referenced",
                    data_type.display(self.resolved_environment)
                )
            }
            SemanticAnalysisError::ExpressionIsNotAPlace => {
                write!(f, "This expression is not a place")
            }
            SemanticAnalysisError::ExpressionIsNotAnAssignee => {
                write!(f, "This expression is not an assignee")
            }
            SemanticAnalysisError::TypeDoesntHaveFields(data_type) => {
                write!(
                    f,
                    "The type `{}` does not have any fields",
                    data_type.display(self.resolved_environment)
                )
            }
            SemanticAnalysisError::CannotNegateType(data_type) => {
                write!(
                    f,
                    "The type `{}` cannot be negated",
                    data_type.display(self.resolved_environment)
                )
            }
            SemanticAnalysisError::CannotInvertType(data_type) => {
                write!(
                    f,
                    "The type `{}` cannot be inverted",
                    data_type.display(self.resolved_environment)
                )
            }
            SemanticAnalysisError::TypeDoesntHaveField { data_type, field } => {
                write!(
                    f,
                    "The type `{}` does not have a field named `{}`",
                    data_type.display(self.resolved_environment),
                    field
                )
            }
            SemanticAnalysisError::MismatchedParameterCount {
                function_name,
                expected,
                actual,
            } => {
                let s = if *expected == 1 { "" } else { "s" };
                let was_were = if *actual == 1 { "was" } else { "were" };

                if let Some(function_name) = function_name {
                    write!(
                        f,
                        "The function `{}` takes {} parameter{} but {} {} given",
                        function_name, expected, s, actual, was_were
                    )
                } else {
                    write!(
                        f,
                        "Epxected {} parameter{} but {} {} given",
                        expected, s, actual, was_were
                    )
                }
            }
            SemanticAnalysisError::InvalidGenerics {
                type_name,
                expected,
                actual,
            } => {
                let s = if *expected == 1 { "" } else { "s" };
                let was_were = if *actual == 1 { "was" } else { "were" };
                write!(
                    f,
                    "The type `{}` takes {} generic argument{} but {} {} given",
                    type_name, expected, s, actual, was_were
                )
            }
            SemanticAnalysisError::MacroConflict => {
                write!(
                    f,
                    "This string conflicts with the compiler-generated command macros"
                )
            }
            SemanticAnalysisError::ControlFlowNotInLoop(kind) => {
                write!(f, "Cannot `{}` outside of a loop", kind.name())
            }
            SemanticAnalysisError::ExpressionIsNotCallable => {
                f.write_str("This expression is not callable")
            }
            SemanticAnalysisError::NotAFunction(name) => write!(f, "`{}` is not a function", name),
            SemanticAnalysisError::NotAType(name) => write!(f, "`{}` is not a type", name),
            SemanticAnalysisError::NotAStruct(name) => {
                write!(f, "The type `{}` is not a struct", name)
            }
            SemanticAnalysisError::NotARegularStruct(name) => {
                write!(f, "The type `{}` is not a regular struct", name)
            }
            SemanticAnalysisError::NotATupleStruct(name) => {
                write!(f, "The type `{}` is not a tuple struct", name)
            }
            SemanticAnalysisError::MismatchedTupleStructFieldCount(name, expected, actual) => {
                let s = if *expected == 1 { "" } else { "s" };
                let was_were = if *actual == 1 { "was" } else { "were" };

                write!(
                    f,
                    "The tuple struct `{}` requires {} field{} but only {} {} passed",
                    name, expected, s, actual, was_were
                )
            }
            SemanticAnalysisError::NotAModule(name) => {
                write!(f, "The type `{}` is not a module", name)
            }
            SemanticAnalysisError::TypeNotPublic(name) => {
                write!(f, "The type `{}` is not public", name)
            }
            SemanticAnalysisError::ValueNotPublic(name) => {
                write!(f, "The value `{}` is not public", name)
            }
            SemanticAnalysisError::UnknownType(name) => write!(f, "Unknown type `{}`", name),
            SemanticAnalysisError::UnknownValue(name) => write!(f, "Unknown value `{}`", name),
            SemanticAnalysisError::UnknownItem(name) => write!(f, "Unknown item `{}`", name),
            SemanticAnalysisError::UnknownModule(name) => {
                write!(f, "Unknown module `{}`", name)
            }
            SemanticAnalysisError::TypeDoesntContainType {
                container_type_name,
                type_name,
            } => write!(
                f,
                "The type `{}` does not contain a type named `{}`",
                container_type_name, type_name
            ),
            SemanticAnalysisError::TypeDoesntContainValue {
                type_name,
                value_name,
            } => write!(
                f,
                "The type `{}` does not contain a value named `{}`",
                type_name, value_name
            ),
            SemanticAnalysisError::TypeDoesntContainItems { type_name } => {
                write!(f, "The type `{}` does not contain any items", type_name)
            }
            SemanticAnalysisError::ModuleDoesntContainItem {
                module_name,
                item_name,
            } => write!(
                f,
                "The module `{}` does not contain an item named `{}`",
                module_name, item_name
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum SemanticAnalysisError {
    InherentImplRequiresNomialType,
    FunctionTypesNotAllRuntime,
    FunctionTypesNotAllData,
    RecursiveFunctionNotRuntime,
    CannotPerformArithmeticOperation {
        left: UnresolvedDataType,
        operator: ArithmeticOperator,
        right: UnresolvedDataType,
    },
    CannotPerformComparisonOperation {
        left: UnresolvedDataType,
        operator: ComparisonOperator,
        right: UnresolvedDataType,
    },
    CannotPerformAugmentedAssignment(UnresolvedDataType),
    MismatchedPatternTypes {
        expected: UnresolvedDataType,
        actual: Box<PatternType>,
    },
    UnderscoreExpression,
    CannotIterateType(UnresolvedDataType),
    MismatchedTypes {
        expected: UnresolvedDataType,
        actual: UnresolvedDataType,
    },
    InvalidAugmentedAssignmentType(ArithmeticOperator, UnresolvedDataType, UnresolvedDataType),
    CannotCastType {
        from: UnresolvedDataType,
        to: UnresolvedDataType,
    },
    CannotUseReturnInCompiletimeFunction,
    CannotBeRepresentedAsFloat(UnresolvedDataType),
    RecursiveFunctionCall,
    CompiletimeValueMutationInRuntimeLoop,
    MissingKey(String),
    UnexpectedKey(String),
    MissingField(String),
    TypeAlreadyDeclared(String),
    ValueAlreadyDeclared(String),
    PatternIsNotIrrefutable,
    TypeIsNotCondition(UnresolvedDataType),
    TypeIsNotScoreCompatible(UnresolvedDataType),
    TypeIsNotDataCompatible(UnresolvedDataType),
    CannotBeAssignedToData(UnresolvedDataType),
    CannotBeIndexed(UnresolvedDataType),
    CannotBeIndexedByType {
        target: UnresolvedDataType,
        index: UnresolvedDataType,
    },
    IndexOutOfBounds,
    CannotBeDereferenced(UnresolvedDataType),
    CannotBeReferenced(UnresolvedDataType),
    ExpressionIsNotAPlace,
    ExpressionIsNotAnAssignee,
    TypeDoesntHaveFields(UnresolvedDataType),
    CannotNegateType(UnresolvedDataType),
    CannotInvertType(UnresolvedDataType),
    TypeDoesntHaveField {
        data_type: UnresolvedDataType,
        field: String,
    },
    MismatchedParameterCount {
        function_name: Option<String>,
        expected: usize,
        actual: usize,
    },
    InvalidGenerics {
        type_name: String,
        expected: usize,
        actual: usize,
    },
    MacroConflict,
    ControlFlowNotInLoop(LoopControlFlowKind),
    ExpressionIsNotCallable,
    NotAFunction(String),
    NotAType(String),
    NotAStruct(String),
    NotARegularStruct(String),
    NotATupleStruct(String),
    MismatchedTupleStructFieldCount(String, usize, usize),
    NotAModule(String),
    TypeNotPublic(String),
    ValueNotPublic(String),
    UnknownType(String),
    UnknownValue(String),
    UnknownItem(String),
    UnknownModule(String),
    TypeDoesntContainType {
        container_type_name: String,
        type_name: String,
    },
    TypeDoesntContainItems {
        type_name: String,
    },
    TypeDoesntContainValue {
        type_name: String,
        value_name: String,
    },
    ModuleDoesntContainItem {
        module_name: String,
        item_name: String,
    },
}

impl SemanticAnalysisError {
    #[must_use]
    pub const fn display<'a>(
        &'a self,
        resolved_environment: &'a ResolvedEnvironment,
    ) -> SemanticAnalysisErrorDisplay<'a> {
        SemanticAnalysisErrorDisplay {
            error: self,
            resolved_environment,
        }
    }
}
