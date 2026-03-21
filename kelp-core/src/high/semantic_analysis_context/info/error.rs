use std::fmt::Write;

use crate::{
    middle::{data_type::DataType, environment::Environment, statement::ControlFlowKind},
    operator::{ArithmeticOperator, ComparisonOperator},
    pattern_type::PatternType,
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
    CannotPerformAugmentedAssignment(DataType),
    MismatchedPatternTypes {
        expected: DataType,
        actual: PatternType,
    },
    UnderscoreExpression,
    CannotIterateType(DataType),
    MismatchedTypes {
        expected: DataType,
        actual: DataType,
    },
    InvalidAugmentedAssignmentType(ArithmeticOperator, DataType, DataType),
    CannotCastType {
        from: DataType,
        to: DataType,
    },
    CannotBeRepresentedAsFloat(DataType),
    UnknownRuntimeStorageType,
    ValueTooLarge(DataType),
    ValueTooSmall(DataType),
    CompiletimeValueMutationInRuntimeLoop,
    TypeIsNotStruct(String),
    MissingKey(String),
    UnexpectedKey(String),
    MissingField(String),
    TypeIsAlreadyDefined(String),
    PatternIsNotIrrefutable,
    TypeIsNotCondition(DataType),
    TypeIsNotScoreCompatible(DataType),
    CannotBeAssignedToData(DataType),
    CannotBeIndexed(DataType),
    IndexOutOfBounds,
    CannotBeDereferenced(DataType),
    CannotBeReferenced(DataType),
    CannotBeAssignedTo,
    TypeDoesntHaveFields(DataType),
    CannotNegateType(DataType),
    CannotInvertType(DataType),
    TypeDoesntHaveField {
        data_type: DataType,
        field: String,
    },
    InvalidGenerics {
        data_type_name: String,
        expected: usize,
        actual: usize,
    },
    MacroConflict,
    ControlFlowNotInLoop(ControlFlowKind),

    NotAType(String),
    NotAModule(String),
    UnknownType(String),
    UnknownValue(String),
    UnknownModule(String),
    ModuleDoesntContainType {
        module_name: String,
        type_name: String,
    },
    ModuleDoesntContainValue {
        module_name: String,
        value_name: String,
    },
}

impl SemanticAnalysisError {
    #[must_use]
    pub fn format_string(&self, environment: &Environment) -> String {
        let mut output = String::new();
        let _ = self.write_string(&mut output, environment);
        output
    }

    pub fn write_string(&self, output: &mut String, environment: &Environment) -> std::fmt::Result {
        match self {
            Self::CannotPerformArithmeticOperation {
                left,
                operator,
                right,
            } => {
                output.write_str("Cannot perform: `")?;
                left.write_string(output, environment)?;
                write!(output, "` {} `", operator)?;
                right.write_string(output, environment)?;
                output.write_str("`")
            }
            Self::CannotPerformComparisonOperation {
                left,
                operator,
                right,
            } => {
                output.write_str("Cannot perform: `")?;
                left.write_string(output, environment)?;
                write!(output, "` {} `", operator)?;
                right.write_string(output, environment)?;
                output.write_str("`")
            }
            Self::CannotPerformAugmentedAssignment(data_type) => {
                output.write_str("Cannot perform augmented assignment on type `")?;
                data_type.write_string(output, environment)?;
                output.write_str("`")
            }
            Self::MismatchedPatternTypes { expected, actual } => {
                output.write_str("Expected type `")?;
                expected.write_string(output, environment)?;
                write!(output, "` but got `{}`", actual)
            }
            Self::UnderscoreExpression => output.write_str(
                "The underscore expression can only be used on the left hand side of assignments",
            ),
            Self::CannotIterateType(data_type) => {
                output.write_str("Cannot iterate over type `")?;
                data_type.write_string(output, environment)?;
                output.write_str("`")
            }
            Self::MismatchedTypes { expected, actual } => {
                output.write_str("Expected type `")?;
                expected.write_string(output, environment)?;
                output.write_str("` but got `")?;
                actual.write_string(output, environment)?;
                output.write_str("`")
            }
            Self::InvalidAugmentedAssignmentType(op, target, value) => {
                write!(output, "Cannot {}-assign type `", op.name())?;
                value.write_string(output, environment)?;
                output.write_str("` to type `")?;
                target.write_string(output, environment)?;
                output.write_str("`")
            }
            Self::CannotCastType { from, to } => {
                output.write_str("Cannot cast type `")?;
                from.write_string(output, environment)?;
                output.write_str("` to `")?;
                to.write_string(output, environment)?;
                output.write_str("`")
            }
            Self::CannotBeRepresentedAsFloat(data_type) => {
                output.write_str("The type `")?;
                data_type.write_string(output, environment)?;
                output.write_str("` cannot be represented as a float")?;

                Ok(())
            }
            Self::UnknownRuntimeStorageType => output.write_str("Unknown runtime storage type"),
            Self::ValueTooLarge(data_type) => {
                output.write_str("This value is too big to fit in the type `")?;
                data_type.write_string(output, environment)?;
                output.write_str("`")
            }
            Self::ValueTooSmall(data_type) => {
                output.write_str("This value is too small to fit in the type `")?;
                data_type.write_string(output, environment)?;
                output.write_str("`")
            }
            Self::CompiletimeValueMutationInRuntimeLoop => {
                output.write_str("Cannot mutate a compile-time value in a runtime loop")
            }
            Self::TypeIsNotStruct(name) => write!(output, "The type `{}` is not a struct", name),
            Self::MissingKey(key) => write!(output, "Missing key `{}`", key),
            Self::UnexpectedKey(key) => write!(output, "Unexpected key `{}`", key),
            Self::MissingField(field) => write!(output, "Missing field `{}`", field),
            Self::TypeIsAlreadyDefined(name) => write!(
                output,
                "The type `{}` has already been declared in this scope",
                name
            ),
            Self::PatternIsNotIrrefutable => output.write_str("This pattern is not irrefutable"),
            Self::TypeIsNotCondition(data_type) => {
                output.write_str("The type `")?;
                data_type.write_string(output, environment)?;
                output.write_str("` cannot be used in conditions")
            }
            Self::TypeIsNotScoreCompatible(data_type) => {
                output.write_str("The type `")?;
                data_type.write_string(output, environment)?;
                output.write_str("` is not score compatible")
            }
            Self::CannotBeAssignedToData(data_type) => {
                output.write_str("The type `")?;
                data_type.write_string(output, environment)?;
                output.write_str("` cannot be assigned to data storage")
            }
            Self::CannotBeIndexed(data_type) => {
                output.write_str("The type `")?;
                data_type.write_string(output, environment)?;
                output.write_str("` cannot be indexed")
            }
            Self::IndexOutOfBounds => output.write_str("Index out of bounds"),
            Self::CannotBeDereferenced(data_type) => {
                output.write_str("The type `")?;
                data_type.write_string(output, environment)?;
                output.write_str("` cannot be dereferenced")
            }
            Self::CannotBeReferenced(data_type) => {
                output.write_str("The type `")?;
                data_type.write_string(output, environment)?;
                output.write_str("` cannot be referenced")
            }
            Self::CannotBeAssignedTo => {
                output.write_str("Cannot assign a value to this expression")
            }
            Self::TypeDoesntHaveFields(data_type) => {
                output.write_str("The type `")?;
                data_type.write_string(output, environment)?;
                output.write_str("` does not have any fields")
            }
            Self::CannotNegateType(data_type) => {
                output.write_str("The type `")?;
                data_type.write_string(output, environment)?;
                output.write_str("` cannot be negated")
            }
            Self::CannotInvertType(data_type) => {
                output.write_str("The type `")?;
                data_type.write_string(output, environment)?;
                output.write_str("` cannot be inverted")
            }
            Self::TypeDoesntHaveField { data_type, field } => {
                output.write_str("The type `")?;
                data_type.write_string(output, environment)?;
                write!(output, "` does not have a field named `{}`", field)
            }
            Self::InvalidGenerics {
                data_type_name,
                expected,
                actual,
            } => {
                let arg_s = if *expected == 1 { "" } else { "s" };
                let was_were = if *actual == 1 { "was" } else { "were" };
                write!(
                    output,
                    "The type `{}` takes {} generic argument{} but {} {} given",
                    data_type_name, expected, arg_s, actual, was_were
                )
            }
            Self::MacroConflict => {
                output.write_str("This string conflicts with the compiler-generated command macros")
            }
            Self::ControlFlowNotInLoop(kind) => {
                write!(output, "Cannot `{}` outside of a loop", kind.name())
            }
            Self::NotAType(name) => write!(output, "`{}` is not a type", name),
            Self::NotAModule(name) => write!(output, "`{}` is not a module", name),
            Self::UnknownType(name) => write!(output, "Unknown type `{}`", name),
            Self::UnknownValue(name) => write!(output, "Unknown value `{}`", name),
            Self::UnknownModule(name) => write!(output, "Unknown module `{}`", name),
            Self::ModuleDoesntContainType {
                module_name,
                type_name,
            } => write!(
                output,
                "The module `{}` does not contain the type `{}`",
                module_name, type_name
            ),
            Self::ModuleDoesntContainValue {
                module_name,
                value_name,
            } => write!(
                output,
                "The module `{}` does not contain the value `{}`",
                module_name, value_name
            ),
        }
    }
}
