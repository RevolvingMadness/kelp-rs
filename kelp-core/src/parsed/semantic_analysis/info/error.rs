use std::fmt::Display;

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

#[derive(Debug, Clone, Copy)]
pub enum ItemKind {
    Type,
    Value,
}

impl Display for ItemKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Type => "type",
            Self::Value => "value",
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TypeKind {
    Module,
    Struct,
    Alias,
    Generic,
    Builtin,
}

impl TypeKind {
    #[must_use]
    pub const fn name(&self) -> &str {
        match self {
            Self::Module => "module",
            Self::Struct => "struct",
            Self::Alias => "alias",
            Self::Generic => "generic",
            Self::Builtin => "builtin",
        }
    }

    #[must_use]
    pub const fn name_plural(&self) -> &str {
        match self {
            Self::Module => "modules",
            Self::Struct => "structs",
            Self::Alias => "type aliases",
            Self::Generic => "type generics",
            Self::Builtin => "builtin types",
        }
    }
}

pub struct SemanticAnalysisErrorDisplay<'a> {
    pub error: &'a SemanticAnalysisError,
    pub semantic_environment: &'a SemanticEnvironment,
}

#[derive(Debug, Clone)]
pub enum SemanticAnalysisError {
    MethodNotInImpl {
        method_name_span: Span,
        self_keyword_span: Span,
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
        left: SemanticDataType,
        operator_span: Span,
        operator: ComparisonOperator,
        right: SemanticDataType,
    },
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
    CannotCastType {
        span: Span,
        from: SemanticDataType,
        to: SemanticDataType,
    },
    CannotUseReturnInCompiletimeFunction {
        function_declaration_span: Span,
        return_keyword_span: Span,
    },
    RecursiveFunctionCall {
        declaration_span: Span,
        span: Span,
    },
    CompiletimeValueMutationInRuntimeLoop {
        value_span: Span,
    },
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
        declaration_span: Option<Span>,
        expected: usize,
        actual: usize,
    },
    InvalidGenerics {
        name_span: Span,
        declaration_span: Option<Span>,
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
        callee_type: SemanticDataType,
    },
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
        type_span: Span,
        type_kind: TypeKind,
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
            Self::MethodNotInImpl {
                method_name_span,
                self_keyword_span,
            } => Diagnostic::error("method not declared in `impl`")
                .with_primary_label(
                    method_name_span,
                    "methods can only be declared in `impl` blocks",
                )
                .with_secondary_label(
                    self_keyword_span,
                    "function is a method because of `self` parameter",
                ),
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
            } => Diagnostic::error("all types of a `runtime` function must be runtime")
                .with_primary_no_label(runtime_keyword_span),
            Self::FunctionTypesNotAllData {
                recursive_keyword_span,
            } => Diagnostic::error("all types of a `recursive` function must be `data`")
                .with_primary_no_label(recursive_keyword_span),
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
            } => Diagnostic::error(format!("invalid operands for operator `{}`", operator))
                .with_primary_label(
                    operator_span,
                    format!(
                        "cannot perform `{}` {} `{}`",
                        left.display(environment),
                        operator,
                        right.display(environment)
                    ),
                ),
            Self::CannotPerformComparisonOperation {
                left,
                operator_span,
                operator,
                right,
            } => Diagnostic::error(format!("invalid operands for operator `{}`", operator))
                .with_primary_label(
                    operator_span,
                    format!(
                        "cannot perform `{}` {} `{}`",
                        left.display(environment),
                        operator,
                        right.display(environment)
                    ),
                ),
            Self::MismatchedPatternTypes {
                span,
                expected,
                actual,
            } => Diagnostic::error("mismatched pattern types").with_primary_label(
                span,
                format!(
                    "expected `{}`, found `{}`",
                    expected.display(environment),
                    actual
                ),
            ),
            Self::UnderscoreExpression { span } => {
                Diagnostic::error("invalid usage of underscore expression")
                    .with_primary_label(span, "underscore expressions can only be used as places")
            }
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
            Self::CannotCastType { span, from, to } => {
                Diagnostic::error("cannot cast between types").with_primary_label(
                    span,
                    format!(
                        "cannot cast from type `{}` to `{}`",
                        from.display(environment),
                        to.display(environment)
                    ),
                )
            }
            Self::CannotUseReturnInCompiletimeFunction {
                function_declaration_span,
                return_keyword_span,
            } => Diagnostic::error("`return` not allowed in compiletime functions")
                .with_primary_no_label(return_keyword_span)
                .with_secondary_label(
                    function_declaration_span,
                    "function declared here without `runtime` modifier",
                ),
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
            .with_secondary_label(declaration_span, "originally declared here"),
            Self::ValueAlreadyDeclared {
                declaration_span,
                redeclaration_span,
                name,
            } => Diagnostic::error(format!(
                "the value `{}` has already been declared in this scope",
                name
            ))
            .with_primary_label(redeclaration_span, "redeclared here")
            .with_secondary_label(declaration_span, "originally declared here"),
            Self::PatternIsNotIrrefutable => todo!(),
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
            } => Diagnostic::error("type is not indexable").with_primary_label(
                type_span,
                format!(
                    "the type `{}` cannot be indexed",
                    data_type.display(environment)
                ),
            ),
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
                declaration_span,
                expected,
                actual,
            } => {
                let mut diagnostic = Diagnostic::error("mismatched argument count")
                    .with_primary_label(
                        callee_span,
                        format!("expected {} argument(s), found {}", expected, actual),
                    );

                if let Some(declaration_span) = declaration_span {
                    diagnostic = diagnostic.with_secondary_label(declaration_span, "declared here");
                }

                diagnostic
            }
            Self::InvalidGenerics {
                name_span: span,
                declaration_span,
                expected,
                actual,
            } => {
                let mut diagnostic = Diagnostic::error("mismatched generic type argument count")
                    .with_primary_label(
                        span,
                        format!(
                            "expected {} generic type argument(s), found {}",
                            expected, actual
                        ),
                    );

                if let Some(declaration_span) = declaration_span {
                    diagnostic = diagnostic.with_secondary_label(
                        declaration_span,
                        format!("declared here with {} generic type parameter(s)", expected),
                    );
                }

                diagnostic
            }
            Self::ControlFlowNotInLoop { span, kind } => {
                Diagnostic::error("loop control flow outside loop")
                    .with_primary_label(span, format!("cannot `{}` outside of a loop", kind))
            }
            Self::ExpressionIsNotCallable {
                callee_span,
                callee_type,
            } => Diagnostic::error("type is not callable").with_primary_label(
                callee_span,
                format!(
                    "the type `{}` cannot be called",
                    callee_type.display(environment)
                ),
            ),
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
            Self::TypeDoesntContainItems {
                type_span,
                type_kind,
            } => Diagnostic::error(format!(
                "{} do not contain any items",
                type_kind.name_plural()
            ))
            .with_primary_no_label(type_span),
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
