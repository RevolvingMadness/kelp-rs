use crate::operator::UnaryOperator;
use crate::parsed::semantic_analysis::info::diagnostic::{Diagnostic, LabelType};
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::SemanticEnvironment;
use crate::span::Span;
use crate::{
    operator::{ArithmeticOperator, ComparisonOperator},
    pattern_type::PatternType,
    semantic::statement::LoopControlFlowKind,
};

pub struct SemanticAnalysisErrorDisplay<'a> {
    pub error: &'a SemanticAnalysisError,
    pub semantic_environment: &'a SemanticEnvironment,
}

// impl Display for SemanticAnalysisErrorDisplay<'_> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match &self.error {
//             Self::InherentImplRequiresNomialType => {
//                 f.write_str("An inherent `impl` requires a nomial type")
//             }
//             Self::FunctionTypesNotAllRuntime => {
//                 f.write_str("Every type in a `runtime` function signature must be runtime")
//             }
//             Self::FunctionTypesNotAllData => {
//                 f.write_str("Every type in a `recursive` function signature must be `data<_>`")
//             }
//             Self::RecursiveFunctionNotRuntime => {
//                 f.write_str("A function marked as `recursive` must also be marked as `runtime`")
//             }
//             Self::CannotPerformArithmeticOperation {
//                 left,
//                 operator,
//                 right,
//             } => {
//                 write!(
//                     f,
//                     "Cannot perform: `{}` {} `{}`",
//                     left.display(self.semantic_environment),
//                     operator,
//                     right.display(self.semantic_environment)
//                 )
//             }
//             Self::CannotPerformComparisonOperation {
//                 left,
//                 operator,
//                 right,
//             } => {
//                 write!(
//                     f,
//                     "Cannot perform: `{}` {} `{}`",
//                     left.display(self.semantic_environment),
//                     operator,
//                     right.display(self.semantic_environment)
//                 )
//             }
//             Self::CannotPerformAugmentedAssignment(data_type) => {
//                 write!(
//                     f,
//                     "Cannot perform augmented assignment on type `{}`",
//                     data_type.display(self.semantic_environment)
//                 )
//             }
//             Self::MismatchedPatternTypes { expected, actual } => {
//                 write!(
//                     f,
//                     "Expected type `{}` but got `{}`",
//                     expected.display(self.semantic_environment),
//                     actual
//                 )
//             }
//             Self::UnderscoreExpression => write!(
//                 f,
//                 "The underscore expression can only be used as an assignee"
//             ),
//             Self::CannotIterateType(data_type) => {
//                 write!(
//                     f,
//                     "Cannot iterate over type `{}`",
//                     data_type.display(self.semantic_environment)
//                 )
//             }
//             Self::MismatchedTypes { expected, actual } => {
//                 write!(
//                     f,
//                     "Expected type `{}` but got `{}`",
//                     expected.display(self.semantic_environment),
//                     actual.display(self.semantic_environment)
//                 )
//             }
//             Self::InvalidAugmentedAssignmentType(op, target, value) => {
//                 write!(
//                     f,
//                     "Cannot {}-assign type `{}` to type `{}`",
//                     op.name(),
//                     value.display(self.semantic_environment),
//                     target.display(self.semantic_environment)
//                 )
//             }
//             Self::CannotCastType { from, to } => {
//                 write!(
//                     f,
//                     "Cannot cast type `{}` to `{}`",
//                     from.display(self.semantic_environment),
//                     to.display(self.semantic_environment)
//                 )
//             }
//             Self::CannotUseReturnInCompiletimeFunction => {
//                 write!(
//                     f,
//                     "`return` can only be used in functions marked as `runtime`"
//                 )
//             }
//             Self::CannotBeRepresentedAsFloat(data_type) => {
//                 write!(
//                     f,
//                     "The type `{}` cannot be represented as a float",
//                     data_type.display(self.semantic_environment)
//                 )
//             }
//             Self::RecursiveFunctionCall => f.write_str(
//                 "Recursive function calls can only be used inside functions marked as `recursive`",
//             ),
//             Self::CompiletimeValueMutationInRuntimeLoop => {
//                 f.write_str("Cannot mutate a compile-time value in a runtime loop")
//             }
//             Self::MissingKey(key) => write!(f, "Missing key `{}`", key),
//             Self::UnexpectedKey(key) => write!(f, "Unexpected key `{}`", key),
//             Self::MissingField(field) => {
//                 write!(f, "Missing field `{}`", field)
//             }
//             Self::TypeAlreadyDeclared(name) => {
//                 write!(
//                     f,
//                     "A type with the name `{}` has already been declared",
//                     name
//                 )
//             }
//             Self::ValueAlreadyDeclared(name) => {
//                 write!(
//                     f,
//                     "A value with the name `{}` has already been declared",
//                     name
//                 )
//             }
//             Self::PatternIsNotIrrefutable => {
//                 write!(f, "This pattern is not irrefutable")
//             }
//             Self::TypeIsNotCondition(data_type) => {
//                 write!(
//                     f,
//                     "The type `{}` cannot be used in conditions",
//                     data_type.display(self.semantic_environment)
//                 )
//             }
//             Self::TypeIsNotScoreCompatible(data_type) => {
//                 write!(
//                     f,
//                     "The type `{}` is not score compatible",
//                     data_type.display(self.semantic_environment)
//                 )
//             }
//             Self::TypeIsNotDataCompatible(data_type) => {
//                 write!(
//                     f,
//                     "The type `{}` is not data compatible",
//                     data_type.display(self.semantic_environment)
//                 )
//             }
//             Self::CannotBeAssignedToData(data_type) => {
//                 write!(
//                     f,
//                     "The type `{}` cannot be assigned to data storage",
//                     data_type.display(self.semantic_environment)
//                 )
//             }
//             Self::CannotBeIndexed(data_type) => {
//                 write!(
//                     f,
//                     "The type `{}` cannot be indexed",
//                     data_type.display(self.semantic_environment)
//                 )
//             }
//             Self::CannotBeIndexedByType { target, index } => {
//                 write!(
//                     f,
//                     "The type `{}` cannot be indexed by `{}`",
//                     target.display(self.semantic_environment),
//                     index.display(self.semantic_environment),
//                 )
//             }
//             Self::IndexOutOfBounds => write!(f, "Index out of bounds"),
//             Self::CannotBeDereferenced(data_type) => {
//                 write!(
//                     f,
//                     "The type `{}` cannot be dereferenced",
//                     data_type.display(self.semantic_environment)
//                 )
//             }
//             Self::CannotBeReferenced(data_type) => {
//                 write!(
//                     f,
//                     "The type `{}` cannot be referenced",
//                     data_type.display(self.semantic_environment)
//                 )
//             }
//             Self::ExpressionIsNotAPlace => {
//                 write!(f, "This expression is not a place")
//             }
//             Self::ExpressionIsNotAnAssignee => {
//                 write!(f, "This expression is not an assignee")
//             }
//             Self::TypeDoesntHaveFields(data_type) => {
//                 write!(
//                     f,
//                     "The type `{}` does not have any fields",
//                     data_type.display(self.semantic_environment)
//                 )
//             }
//             Self::CannotNegateType(data_type) => {
//                 write!(
//                     f,
//                     "The type `{}` cannot be negated",
//                     data_type.display(self.semantic_environment)
//                 )
//             }
//             Self::CannotInvertType(data_type) => {
//                 write!(
//                     f,
//                     "The type `{}` cannot be inverted",
//                     data_type.display(self.semantic_environment)
//                 )
//             }
//             Self::TypeDoesntHaveField { data_type, field } => {
//                 write!(
//                     f,
//                     "The type `{}` does not have a field named `{}`",
//                     data_type.display(self.semantic_environment),
//                     field
//                 )
//             }
//             Self::MismatchedParameterCount {
//                 function_name,
//                 expected,
//                 actual,
//             } => {
//                 let s = if *expected == 1 { "" } else { "s" };
//                 let was_were = if *actual == 1 { "was" } else { "were" };

//                 if let Some(function_name) = function_name {
//                     write!(
//                         f,
//                         "The function `{}` takes {} parameter{} but {} {} given",
//                         function_name, expected, s, actual, was_were
//                     )
//                 } else {
//                     write!(
//                         f,
//                         "Epxected {} parameter{} but {} {} given",
//                         expected, s, actual, was_were
//                     )
//                 }
//             }
//             Self::InvalidGenerics {
//                 type_name,
//                 expected,
//                 actual,
//             } => {
//                 let s = if *expected == 1 { "" } else { "s" };
//                 let was_were = if *actual == 1 { "was" } else { "were" };
//                 write!(
//                     f,
//                     "`{}` takes {} generic argument{} but {} {} given",
//                     type_name, expected, s, actual, was_were
//                 )
//             }
//             Self::MacroConflict => {
//                 write!(
//                     f,
//                     "This string conflicts with the compiler-generated command macros"
//                 )
//             }
//             Self::ControlFlowNotInLoop(kind) => {
//                 write!(f, "Cannot `{}` outside of a loop", kind.name())
//             }
//             Self::ExpressionIsNotCallable => f.write_str("This expression is not callable"),
//             Self::NotAFunction(name) => write!(f, "`{}` is not a function", name),
//             Self::NotAType(name) => write!(f, "`{}` is not a type", name),
//             Self::NotAStruct(data_type) => {
//                 write!(
//                     f,
//                     "The type `{}` is not a struct",
//                     data_type.display(self.semantic_environment)
//                 )
//             }
//             Self::NotARegularStruct(data_type) => {
//                 write!(
//                     f,
//                     "The type `{}` is not a regular struct",
//                     data_type.display(self.semantic_environment)
//                 )
//             }
//             Self::NotATupleStruct(data_type) => {
//                 write!(
//                     f,
//                     "The type `{}` is not a tuple struct",
//                     data_type.display(self.semantic_environment)
//                 )
//             }
//             Self::MismatchedTupleStructFieldCount(name, expected, actual) => {
//                 let s = if *expected == 1 { "" } else { "s" };
//                 let was_were = if *actual == 1 { "was" } else { "were" };

//                 write!(
//                     f,
//                     "The tuple struct `{}` requires {} field{} but only {} {} passed",
//                     name, expected, s, actual, was_were
//                 )
//             }
//             Self::NotAModule(name) => {
//                 write!(f, "The type `{}` is not a module", name)
//             }
//             Self::TypeNotPublic(name) => {
//                 write!(f, "The type `{}` is not public", name)
//             }
//             Self::ValueNotPublic(name) => {
//                 write!(f, "The value `{}` is not public", name)
//             }
//             Self::UnknownType(name) => write!(f, "Unknown type `{}`", name),
//             Self::UnknownValue(name) => write!(f, "Unknown value `{}`", name),
//             Self::UnknownItem(name) => write!(f, "Unknown item `{}`", name),
//             Self::UnknownModule(name) => {
//                 write!(f, "Unknown module `{}`", name)
//             }
//             Self::TypeDoesntContainType {
//                 container_type_name,
//                 type_name,
//             } => write!(
//                 f,
//                 "The type `{}` does not contain a type named `{}`",
//                 container_type_name, type_name
//             ),
//             Self::TypeDoesntContainValue {
//                 type_name,
//                 value_name,
//             } => write!(
//                 f,
//                 "The type `{}` does not contain a value named `{}`",
//                 type_name, value_name
//             ),
//             Self::MethodNotFound {
//                 type_name,
//                 method_name,
//             } => write!(
//                 f,
//                 "The type `{}` does not contain a method named `{}`",
//                 type_name.display(self.semantic_environment),
//                 method_name
//             ),
//             Self::TypeDoesntContainItems { type_name } => {
//                 write!(f, "The type `{}` does not contain any items", type_name)
//             }
//             Self::ModuleDoesntContainItem {
//                 module_name,
//                 item_name,
//             } => write!(
//                 f,
//                 "The module `{}` does not contain an item named `{}`",
//                 module_name, item_name
//             ),
//             Self::MethodNotInImpl => {
//                 f.write_str("Functions with a `self` parameter can only be used in `impl` blocks")
//             }
//         }
//     }
// }

#[derive(Debug, Clone)]
pub enum SemanticAnalysisError {
    MethodNotInImpl {
        span: Span,
    },
    InherentImplRequiresNomialType {
        type_span: Span,
        data_type: SemanticDataType,
    },
    FunctionTypesNotAllRuntime {
        runtime_keyword_span: Span,
    },
    FunctionTypesNotAllData {
        recursive_keyword_span: Span,
    },
    RecursiveFunctionNotRuntime {
        keyword_span: Span,
    },
    CannotPerformArithmeticOperation {
        left: SemanticDataType,
        operator_span: Span,
        operator: ArithmeticOperator,
        right: SemanticDataType,
    },
    CannotPerformComparisonOperation {
        span: Span,
        left: SemanticDataType,
        operator: ComparisonOperator,
        right: SemanticDataType,
    },
    CannotPerformAugmentedAssignment(SemanticDataType),
    MismatchedPatternTypes {
        span: Span,
        expected: SemanticDataType,
        actual: Box<PatternType>,
    },
    UnderscoreExpression {
        span: Span,
    },
    CannotIterateType {
        type_span: Span,
        data_type: SemanticDataType,
    },
    MismatchedTypes {
        span: Span,
        expected: SemanticDataType,
        actual: SemanticDataType,
    },
    InvalidAugmentedAssignmentType(ArithmeticOperator, SemanticDataType, SemanticDataType),
    CannotCastType {
        span: Span,
        from: SemanticDataType,
        to: SemanticDataType,
    },
    CannotUseReturnInCompiletimeFunction {
        function_declaration_span: Span,
        return_keyword_span: Span,
    },
    CannotBeRepresentedAsFloat {
        type_span: Span,
        data_type: SemanticDataType,
    },
    RecursiveFunctionCall {
        declaration_span: Span,
        span: Span,
    },
    CompiletimeValueMutationInRuntimeLoop {
        value_span: Span,
    },
    MissingKey(String),
    UnexpectedKey(String),
    MissingField {
        span: Span,
        name: String,
    },
    TypeAlreadyDeclared {
        declaration_span: Span,
        redeclaration_span: Span,
        name: String,
    },
    ValueAlreadyDeclared {
        declaration_span: Span,
        redeclaration_span: Span,
        name: String,
    },
    PatternIsNotIrrefutable,
    TypeIsNotCondition {
        type_span: Span,
        data_type: SemanticDataType,
    },
    TypeIsNotScoreCompatible {
        type_span: Span,
        data_type: SemanticDataType,
    },
    TypeIsNotDataCompatible {
        type_span: Span,
        data_type: SemanticDataType,
    },
    CannotBeAssignedToData(SemanticDataType),
    CannotBeIndexed {
        type_span: Span,
        data_type: SemanticDataType,
    },
    TypeCannotBeIndexedByType {
        target_span: Span,
        target: SemanticDataType,
        index: SemanticDataType,
    },
    IndexOutOfBounds,
    CannotBeDereferenced {
        type_span: Span,
        data_type: SemanticDataType,
    },
    CannotBeReferenced(SemanticDataType),
    ExpressionIsNotAPlace {
        span: Span,
    },
    TypeDoesntHaveFields {
        type_span: Span,
        data_type: SemanticDataType,
    },
    CannotApplyUnaryOperatorToType {
        operator_span: Span,
        operator: UnaryOperator,
        type_span: Span,
        data_type: SemanticDataType,
    },
    TypeDoesntHaveField {
        data_type: SemanticDataType,
        field_span: Span,
        field: String,
    },
    MismatchedParameterCount {
        callee_span: Span,
        callee_name: Option<String>,
        expected: usize,
        actual: usize,
    },
    InvalidGenerics {
        type_span: Span,
        type_name: String,
        expected: usize,
        actual: usize,
    },
    // MacroConflict,
    ControlFlowNotInLoop {
        span: Span,
        kind: LoopControlFlowKind,
    },
    ExpressionIsNotCallable {
        callee_span: Span,
    },
    NotAFunction(String),
    NotAType {
        type_span: Span,
        type_name: String,
    },
    NotAStruct {
        type_span: Span,
        data_type: SemanticDataType,
    },
    NotARegularStruct {
        type_span: Span,
        data_type: SemanticDataType,
    },
    NotATupleStruct {
        type_span: Span,
        data_type: SemanticDataType,
    },
    MismatchedTupleStructFieldCount {
        name_span: Span,
        name: String,
        expected: usize,
        actual: usize,
    },
    NotAModule {
        span: Span,
        name: String,
    },
    TypeNotPublic(String),
    ValueNotPublic(String),
    UnknownType {
        span: Span,
        name: String,
    },
    UnknownValue {
        span: Span,
        name: String,
    },
    UnknownItem {
        span: Span,
        name: String,
    },
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
    MethodNotFound {
        type_span: Span,
        type_: SemanticDataType,
        method_name: String,
    },
    ModuleDoesntContainItem {
        module_name: String,
        item_name: String,
    },
}

impl SemanticAnalysisError {
    #[must_use]
    pub fn into_diagnostic(self, environment: &SemanticEnvironment) -> Diagnostic {
        match self {
            Self::MethodNotInImpl { span } => todo!(),
            Self::InherentImplRequiresNomialType {
                type_span,
                data_type,
            } => Diagnostic::error("invalid `impl` type").with_primary_label(
                type_span,
                format!(
                    "implementations cannot be defined for type `{}`",
                    data_type.display(environment)
                ),
            ),
            Self::FunctionTypesNotAllRuntime {
                runtime_keyword_span,
            } => Diagnostic::error(
                "all types of a function marked as `runtime` must also be runtime",
            )
            .with_primary_no_label(runtime_keyword_span),
            Self::FunctionTypesNotAllData {
                recursive_keyword_span,
            } => todo!(),
            Self::RecursiveFunctionNotRuntime { keyword_span } => {
                Diagnostic::error("`recursive` functions must also be marked as `runtime`")
                    .with_primary_label(
                        keyword_span,
                        "expected `runtime` modifier alongside the existing `recurisve` modifier",
                    )
            }
            Self::CannotPerformArithmeticOperation {
                left,
                operator_span,
                operator,
                right,
            } => todo!(),
            Self::CannotPerformComparisonOperation {
                span,
                left,
                operator,
                right,
            } => todo!(),
            Self::CannotPerformAugmentedAssignment(semantic_data_type) => todo!(),
            Self::MismatchedPatternTypes {
                span,
                expected,
                actual,
            } => todo!(),
            Self::UnderscoreExpression { span } => todo!(),
            Self::CannotIterateType {
                type_span,
                data_type,
            } => Diagnostic::error("cannot iterate over type").with_primary_label(
                type_span,
                format!(
                    "the type `{}` cannot be iterated over",
                    data_type.display(environment)
                ),
            ),
            Self::MismatchedTypes {
                span,
                expected,
                actual,
            } => Diagnostic::error("mismatched types").with_primary_label(
                span,
                format!(
                    "expected `{}`, found `{}`",
                    expected.display(environment),
                    actual.display(environment)
                ),
            ),
            Self::InvalidAugmentedAssignmentType(
                arithmetic_operator,
                semantic_data_type,
                semantic_data_type1,
            ) => todo!(),
            Self::CannotCastType { span, from, to } => todo!(),
            Self::CannotUseReturnInCompiletimeFunction {
                function_declaration_span,
                return_keyword_span,
            } => Diagnostic::error("`return` not allowed in compiletime functions")
                .with_primary_no_label(return_keyword_span)
                .with_secondary_label(
                    function_declaration_span,
                    "function declared here without `runtime` modifier",
                ),
            Self::CannotBeRepresentedAsFloat {
                type_span,
                data_type,
            } => todo!(),
            Self::RecursiveFunctionCall {
                declaration_span,
                span,
            } => Diagnostic::error(
                "recursive function calls are only allowed within functions marked as `recursive`",
            )
            .with_primary_label(span, "recursive call here")
            .with_secondary_label(
                declaration_span,
                "function declared here without `recursive` modifier",
            ),
            Self::CompiletimeValueMutationInRuntimeLoop { value_span } => todo!(),
            Self::MissingKey(_) => todo!(),
            Self::UnexpectedKey(_) => todo!(),
            Self::MissingField { span, name } => {
                Diagnostic::error(format!("missing required field `{}`", name))
                    .with_primary_no_label(span)
            }
            Self::TypeAlreadyDeclared {
                declaration_span,
                redeclaration_span,
                name,
            } => Diagnostic::error(format!(
                "the type `{}` has already been declared in this scope",
                name
            ))
            .with_primary_label(redeclaration_span, "redeclared here")
            .with_secondary_label(declaration_span, "declared here"),
            Self::ValueAlreadyDeclared {
                declaration_span,
                redeclaration_span: span,
                name,
            } => todo!(),
            Self::PatternIsNotIrrefutable => todo!(),
            Self::TypeIsNotCondition {
                type_span,
                data_type,
            } => todo!(),
            Self::TypeIsNotScoreCompatible {
                type_span,
                data_type,
            } => todo!(),
            Self::TypeIsNotDataCompatible {
                type_span,
                data_type,
            } => todo!(),
            Self::CannotBeAssignedToData(semantic_data_type) => todo!(),
            Self::CannotBeIndexed {
                type_span,
                data_type,
            } => todo!(),
            Self::TypeCannotBeIndexedByType {
                target_span,
                target,
                index,
            } => todo!(),
            Self::IndexOutOfBounds => todo!(),
            Self::CannotBeDereferenced {
                type_span,
                data_type,
            } => todo!(),
            Self::CannotBeReferenced(semantic_data_type) => todo!(),
            Self::ExpressionIsNotAPlace { span } => {
                Diagnostic::error("expression is not a place").with_primary_no_label(span)
            }
            Self::TypeDoesntHaveFields {
                type_span,
                data_type,
            } => todo!(),
            Self::CannotApplyUnaryOperatorToType {
                operator_span,
                operator,
                type_span,
                data_type,
            } => Diagnostic::error(format!(
                "Cannot apply unary operator `{}` to type `{}`",
                operator,
                data_type.display(environment)
            ))
            .with_primary_no_label(operator_span),
            Self::TypeDoesntHaveField {
                data_type,
                field_span,
                field,
            } => todo!(),
            Self::MismatchedParameterCount {
                callee_span,
                callee_name,
                expected,
                actual,
            } => todo!(),
            Self::InvalidGenerics {
                type_span,
                type_name,
                expected,
                actual,
            } => todo!(),
            Self::ControlFlowNotInLoop { span, kind } => todo!(),
            Self::ExpressionIsNotCallable { callee_span } => todo!(),
            Self::NotAFunction(_) => todo!(),
            Self::NotAType {
                type_span,
                type_name,
            } => todo!(),
            Self::NotAStruct {
                type_span,
                data_type,
            } => todo!(),
            Self::NotARegularStruct {
                type_span,
                data_type,
            } => todo!(),
            Self::NotATupleStruct {
                type_span,
                data_type,
            } => todo!(),
            Self::MismatchedTupleStructFieldCount {
                name_span,
                name,
                expected,
                actual,
            } => todo!(),
            Self::NotAModule { span, name } => todo!(),
            Self::TypeNotPublic(_) => todo!(),
            Self::ValueNotPublic(_) => todo!(),
            Self::UnknownType { span, name } => {
                Diagnostic::error(format!("cannot find type `{}` in this scope", name))
                    .with_primary_no_label(span)
            }
            Self::UnknownValue { span, name } => {
                Diagnostic::error(format!("cannot find value `{}` in this scope", name))
                    .with_primary_no_label(span)
            }
            Self::UnknownItem { span, name } => todo!(),
            Self::UnknownModule(_) => todo!(),
            Self::TypeDoesntContainType {
                container_type_name,
                type_name,
            } => todo!(),
            Self::TypeDoesntContainItems { type_name } => todo!(),
            Self::TypeDoesntContainValue {
                type_name,
                value_name,
            } => todo!(),
            Self::MethodNotFound {
                type_span,
                type_,
                method_name,
            } => Diagnostic::error("Method not found").with_label(
                type_span,
                format!("method not found in `{}`", type_.display(environment),),
                LabelType::Primary,
            ),
            Self::ModuleDoesntContainItem {
                module_name,
                item_name,
            } => todo!(),
        }
    }
}
