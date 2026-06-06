use std::fmt::Display;

use crate::operator::UnaryOperator;
use crate::parsed::semantic_analysis::info::diagnostic::{Diagnostic, LabelType};
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::SemanticEnvironment;
use crate::semantic::environment::r#type::r#struct::tuple::HighTupleStructId;
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
pub enum BothItemKinds {
    Type(TypeKind),
    Value(ValueKind),
}

impl From<TypeKind> for BothItemKinds {
    fn from(value: TypeKind) -> Self {
        Self::Type(value)
    }
}

impl From<ValueKind> for BothItemKinds {
    fn from(value: ValueKind) -> Self {
        Self::Value(value)
    }
}

impl BothItemKinds {
    #[must_use]
    pub const fn name(&self) -> &str {
        match self {
            Self::Type(kind) => kind.name(),
            Self::Value(kind) => kind.name(),
        }
    }

    #[must_use]
    pub const fn name_plural(&self) -> &str {
        match self {
            Self::Type(kind) => kind.name_plural(),
            Self::Value(kind) => kind.name_plural(),
        }
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
            Self::Alias => "type alias",
            Self::Generic => "type generic",
            Self::Builtin => "builtin type",
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

#[derive(Debug, Clone, Copy)]
pub enum ValueKind {
    Variable,
    Constant,
    Function,
}

impl ValueKind {
    #[must_use]
    pub const fn name(&self) -> &str {
        match self {
            Self::Variable => "variable",
            Self::Constant => "constant",
            Self::Function => "function",
        }
    }

    #[must_use]
    pub const fn name_plural(&self) -> &str {
        match self {
            Self::Variable => "variables",
            Self::Constant => "constants",
            Self::Function => "functions",
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
    TypeIsNotScoreCompatible {
        type_span: Span,
        data_type: SemanticDataType,
    },
    TypeIsNotDataCompatible {
        type_span: Span,
        data_type: SemanticDataType,
    },
    CannotBeIndexed {
        type_span: Span,
        data_type: SemanticDataType,
    },
    TypeCannotBeIndexedByType {
        target_span: Span,
        target: SemanticDataType,
        index: SemanticDataType,
    },
    CannotBeDereferenced {
        type_span: Span,
        data_type: SemanticDataType,
    },
    ExpressionIsNotAPlace {
        span: Span,
    },
    FieldAccessNotAllowed {
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
        type_declaration_span_and_kind: Option<(Span, TypeKind)>,
        data_type: SemanticDataType,
        field_span: Span,
        field_name: String,
    },
    CompoundDoesntHaveKey {
        data_type: SemanticDataType,
        key_span: Span,
        key_name: String,
    },
    MismatchedArgumentCount {
        callee_span: Span,
        declaration_span: Option<Span>,
        expected: usize,
        actual: usize,
    },
    InvalidGenerics {
        type_name_span: Span,
        item_kind: BothItemKinds,
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
        span: Span,
        kind: TypeKind,
    },
    NotAStruct {
        type_span: Span,
        data_type: SemanticDataType,
    },
    NotARegularStruct {
        declaration_span: Option<Span>,
        type_span: Span,
        data_type: SemanticDataType,
    },
    NotATupleStruct {
        declaration_span: Option<Span>,
        type_span: Span,
        data_type: SemanticDataType,
    },
    MismatchedTupleStructFieldCount {
        name_span: Span,
        struct_id: HighTupleStructId,
        expected: usize,
        actual: usize,
    },
    NotAModule {
        span: Span,
        type_kind: TypeKind,
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
    TypeDoesntContainType {
        container_type_declaration_span: Option<Span>,
        container_type_kind: TypeKind,
        container_type_name: String,
        type_span: Span,
        type_name: String,
    },
    TypeDoesntContainValue {
        type_declaration_span: Option<Span>,
        type_kind: TypeKind,
        type_name: String,
        value_span: Span,
        value_name: String,
    },
    TypeDoesntContainItems {
        type_span: Span,
        type_kind: TypeKind,
    },
    MethodNotFound {
        type_span: Span,
        type_: SemanticDataType,
        method_name: String,
    },
    NotAMethod {
        type_span: Span,
        associated_function_name: String,
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
                Diagnostic::error("`recursive` functions must also be marked `runtime`")
                    .with_primary_no_label(keyword_span)
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
            Self::CompiletimeValueMutationInRuntimeLoop { value_span } => {
                Diagnostic::error("compiletime value mutation in runtime loop").with_primary_label(
                    value_span,
                    "cannot mutate a compiletime variable while in a runtime loop",
                )
            }
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
            Self::TypeIsNotScoreCompatible {
                type_span,
                data_type,
            } => Diagnostic::error("type not score compatible").with_primary_label(
                type_span,
                format!(
                    "the type `{}` cannot be assigned to a score",
                    data_type.display(environment)
                ),
            ),
            Self::TypeIsNotDataCompatible {
                type_span,
                data_type,
            } => Diagnostic::error("type not data compatible").with_primary_label(
                type_span,
                format!(
                    "the type `{}` cannot be assigned to a data storage",
                    data_type.display(environment)
                ),
            ),
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
            Self::CannotBeDereferenced {
                type_span,
                data_type,
            } => Diagnostic::error("type cannot be dereferenced").with_primary_label(
                type_span,
                format!(
                    "the type `{}` cannot be dereferenced",
                    data_type.display(environment)
                ),
            ),
            Self::ExpressionIsNotAPlace { span } => {
                Diagnostic::error("expression is not a place").with_primary_no_label(span)
            }
            Self::FieldAccessNotAllowed {
                type_span,
                data_type,
            } => Diagnostic::error("field access not allowed on type").with_primary_label(
                type_span,
                format!(
                    "the type `{}` does not have fields",
                    data_type.display(environment)
                ),
            ),
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
                type_declaration_span_and_kind,
                data_type,
                field_span,
                field_name,
            } => {
                let mut diagnostic = Diagnostic::error("field not found on type")
                    .with_primary_label(
                        field_span,
                        format!(
                            "the type `{}` does not have a field named `{}`",
                            data_type.display(environment),
                            field_name
                        ),
                    );

                if let Some((span, kind)) = type_declaration_span_and_kind {
                    diagnostic = diagnostic
                        .with_secondary_label(span, format!("{} declared here", kind.name()));
                }

                diagnostic
            }
            Self::CompoundDoesntHaveKey {
                data_type,
                key_span,
                key_name,
            } => Diagnostic::error("key not found in compound").with_primary_label(
                key_span,
                format!(
                    "the compound `{}` does not have a key named `{}`",
                    data_type.display(environment),
                    key_name
                ),
            ),
            Self::MismatchedArgumentCount {
                callee_span,
                declaration_span,
                expected,
                actual,
            } => Diagnostic::error("mismatched argument count")
                .with_primary_label(
                    callee_span,
                    format!("expected {} argument(s), found {}", expected, actual),
                )
                .with_optional_secondary_label(declaration_span, "declared here"),
            Self::InvalidGenerics {
                type_name_span,
                item_kind: type_kind,
                declaration_span,
                expected,
                actual,
            } => Diagnostic::error("mismatched generic count")
                .with_primary_label(
                    type_name_span,
                    format!("expected {} generic(s), found {}", expected, actual),
                )
                .with_optional_secondary_label(
                    declaration_span,
                    format!("{} declared here", type_kind.name()),
                ),
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
            Self::NotAType { span, kind } => {
                Diagnostic::error(format!("expected type, found {}", kind.name()))
                    .with_primary_label(span, "not a type")
            }
            Self::NotAStruct {
                type_span,
                data_type,
            } => Diagnostic::error(format!(
                "the type `{}` is not a struct",
                data_type.display(environment)
            ))
            .with_primary_no_label(type_span),
            Self::NotARegularStruct {
                declaration_span,
                type_span,
                data_type,
            } => Diagnostic::error(format!(
                "the type `{}` is not a regular struct",
                data_type.display(environment)
            ))
            .with_primary_label(type_span, "expected regular struct, found tuple struct")
            .with_optional_secondary_label(declaration_span, "struct declared here"),
            Self::NotATupleStruct {
                declaration_span,
                type_span,
                data_type,
            } => Diagnostic::error(format!(
                "the type `{}` is not a tuple struct",
                data_type.display(environment)
            ))
            .with_primary_label(type_span, "expected tuple struct, found regular struct")
            .with_optional_secondary_label(declaration_span, "struct declared here"),
            Self::MismatchedTupleStructFieldCount {
                name_span,
                struct_id: name,
                expected,
                actual,
            } => todo!(),
            Self::NotAModule { span, type_kind } => Diagnostic::error("type not a module")
                .with_primary_label(span, format!("expected module, found {}", type_kind.name())),
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
            Self::TypeDoesntContainType {
                container_type_declaration_span,
                container_type_kind,
                container_type_name,
                type_span,
                type_name,
            } => Diagnostic::error(format!(
                "type `{}` not found in {} `{}`",
                type_name,
                container_type_kind.name(),
                container_type_name
            ))
            .with_primary_no_label(type_span)
            .with_optional_secondary_label(
                container_type_declaration_span,
                format!("{} declared here", container_type_kind.name()),
            ),
            Self::TypeDoesntContainValue {
                type_declaration_span,
                type_kind,
                type_name,
                value_span,
                value_name,
            } => Diagnostic::error(format!(
                "value `{}` not found in {} `{}`",
                value_name,
                type_kind.name(),
                type_name
            ))
            .with_primary_no_label(value_span)
            .with_optional_secondary_label(
                type_declaration_span,
                format!("{} declared here", type_kind.name()),
            ),
            Self::TypeDoesntContainItems {
                type_span,
                type_kind,
            } => Diagnostic::error(format!(
                "{} do not contain any items",
                type_kind.name_plural()
            ))
            .with_primary_no_label(type_span),
            Self::MethodNotFound {
                type_span,
                type_,
                method_name,
            } => Diagnostic::error("method not found").with_label(
                type_span,
                format!("method not found in type `{}`", type_.display(environment),),
                LabelType::Primary,
            ),
            Self::NotAMethod {
                type_span,
                associated_function_name,
            } => Diagnostic::error("associated function not a method").with_primary_label(
                type_span,
                format!(
                    "`{}` is an associated function, not a method",
                    associated_function_name
                ),
            ),
            Self::ModuleDoesntContainItem {
                module_name,
                item_name,
            } => todo!(),
        }
    }
}
