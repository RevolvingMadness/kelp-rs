use std::{
    collections::{BTreeMap, HashMap},
    fmt::Write,
};

use crate::{
    builtin_data_type::BuiltinDataType,
    high::{
        environment::{
            HighEnvironment,
            r#type::{HighTypeDeclaration, HighTypeId, r#struct::HighStructDeclaration},
        },
        statement::ControlFlowKind,
    },
    middle::{
        data_type::DataType,
        environment::{
            Environment,
            r#type::r#struct::{StructDeclaration, StructId},
            value::{ValueDeclaration, ValueId, variable::VariableDeclaration},
        },
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
    UnknownType(String),
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
    UndeclaredVariable(String),
    MacroConflict,
    ControlFlowNotInLoop(ControlFlowKind),
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
                write!(
                    output,
                    "Cannot {}-assign type `",
                    format_arithmetic_operator(*op)
                )?;
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
            Self::UnknownType(name) => write!(output, "Unknown type `{}`", name),
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
            Self::UndeclaredVariable(name) => write!(
                output,
                "The variable `{}` has not been declared in the current scope",
                name
            ),
            Self::MacroConflict => {
                output.write_str("This string conflicts with the compiler-generated command macros")
            }
            Self::ControlFlowNotInLoop(kind) => {
                write!(
                    output,
                    "Cannot `{}` outside of a loop",
                    format_control_flow_kind(*kind)
                )
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
    pub span: Span,
    pub kind: SemanticAnalysisInfoKind,
}

#[derive(Debug, Default, Clone)]
pub struct Scope {
    pub types: BTreeMap<String, HighTypeId>,
    pub values: BTreeMap<String, ValueId>,
}

impl Scope {
    #[inline]
    pub fn declare_type(&mut self, name: String, id: HighTypeId) {
        self.types.insert(name, id);
    }

    #[inline]
    pub fn declare_value(&mut self, name: String, id: ValueId) {
        self.values.insert(name, id);
    }
}

#[derive(Debug, Clone)]
pub struct SemanticAnalysisContext {
    pub infos: Vec<SemanticAnalysisInfo>,
    pub max_infos: usize,
    pub environment: Environment,
    pub high_environment: HighEnvironment,
    pub scopes: Vec<Scope>,
    pub monomorphized_structs: BTreeMap<(HighTypeId, Vec<DataType>), StructId>,
    pub loop_depth: u32,
    pub is_lhs: bool,
}

impl SemanticAnalysisContext {
    #[must_use]
    pub fn new(max_infos: usize) -> Self {
        let mut self_ = Self {
            infos: Vec::new(),
            max_infos,
            environment: Environment::default(),
            high_environment: HighEnvironment::default(),
            scopes: vec![Scope::default()],
            monomorphized_structs: BTreeMap::new(),
            loop_depth: 0,
            is_lhs: false,
        };

        self_.declare_builtin_type(BuiltinDataType::Boolean);
        self_.declare_builtin_type(BuiltinDataType::Byte);
        self_.declare_builtin_type(BuiltinDataType::Short);
        self_.declare_builtin_type(BuiltinDataType::Integer);
        self_.declare_builtin_type(BuiltinDataType::Long);
        self_.declare_builtin_type(BuiltinDataType::Float);
        self_.declare_builtin_type(BuiltinDataType::Double);
        self_.declare_builtin_type(BuiltinDataType::String);
        // self_.declare_builtin_type(BuiltinDataType::Unit);
        self_.declare_builtin_type(BuiltinDataType::Score);
        self_.declare_builtin_type(BuiltinDataType::List);
        self_.declare_builtin_type(BuiltinDataType::Compound);
        self_.declare_builtin_type(BuiltinDataType::Data);
        self_.declare_builtin_type(BuiltinDataType::SNBT);

        self_
    }

    pub fn get_monomorphized_struct_id(
        &mut self,
        id: HighTypeId,
        declaration: HighStructDeclaration,
        generic_types: Vec<DataType>,
        name_span: Span,
    ) -> Option<StructId> {
        // TODO optimize?
        let id = if let Some(&id) = self.monomorphized_structs.get(&(id, generic_types.clone())) {
            id
        } else {
            let expected_generics = declaration.generic_names.len();
            let actual_generics = generic_types.len();

            if actual_generics != expected_generics {
                return self.add_invalid_generics(
                    name_span,
                    declaration.name,
                    expected_generics,
                    actual_generics,
                );
            }

            let generic_mapping = declaration
                .generic_names
                .into_iter()
                .zip(generic_types.iter().cloned())
                .collect::<HashMap<_, _>>();

            let monomorphized_struct_name = declaration.name.clone();

            let monomorphized_field_types = declaration
                .field_types
                .into_iter()
                .map(|(field_name, field_type)| {
                    let field_type = field_type?.lower(self, &generic_mapping).unwrap();

                    Some((field_name, field_type))
                })
                .collect::<Option<_>>()?;

            self.environment.declare_struct(
                monomorphized_struct_name,
                generic_types,
                monomorphized_field_types,
            )
        };

        Some(id)
    }

    #[inline]
    pub fn add_invalid_generics<T>(
        &mut self,
        span: Span,
        data_type_name: String,
        expected: usize,
        actual: usize,
    ) -> Option<T> {
        self.add_error(
            span,
            SemanticAnalysisError::InvalidGenerics {
                data_type_name,
                expected,
                actual,
            },
        )
    }

    pub fn add_info<T>(&mut self, info: SemanticAnalysisInfo) -> Option<T> {
        if self.infos.len() >= self.max_infos {
            return None;
        }

        self.infos.push(info);

        None
    }

    pub fn add_error<T>(&mut self, span: Span, error: SemanticAnalysisError) -> Option<T> {
        self.add_info(SemanticAnalysisInfo {
            span,
            kind: SemanticAnalysisInfoKind::Error(error),
        })
    }

    pub fn declare_value(&mut self, declaration: ValueDeclaration) -> ValueId {
        let name = declaration.name().to_owned();

        let id = self.environment.declare_value(declaration);

        self.scopes
            .last_mut()
            .expect("No scopes")
            .declare_value(name, id);

        id
    }

    pub fn declare_variable(&mut self, name: String, data_type: Option<DataType>) -> ValueId {
        self.declare_value(ValueDeclaration::Variable(VariableDeclaration {
            name,
            data_type,
        }))
    }

    #[inline]
    pub fn declare_variable_known(&mut self, name: String, data_type: DataType) -> ValueId {
        self.declare_variable(name, Some(data_type))
    }

    #[inline]
    pub fn declare_variable_unknown(&mut self, name: String) -> ValueId {
        self.declare_variable(name, None)
    }

    #[must_use]
    pub fn get_value_id(&self, name: &str) -> Option<ValueId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.values.get(name))
            .copied()
    }

    #[must_use]
    pub fn get_value(&self, id: ValueId) -> &ValueDeclaration {
        &self.environment.values[id.0]
    }

    #[must_use]
    pub fn type_is_declared_in_current_scope(&self, name: &str) -> bool {
        self.scopes.last().unwrap().types.contains_key(name)
    }

    pub fn declare_data_type(&mut self, declaration: HighTypeDeclaration) -> HighTypeId {
        let name = declaration.name().to_owned();

        let id = self.high_environment.declare_type(declaration);

        self.scopes
            .last_mut()
            .expect("No scopes")
            .declare_type(name, id);

        id
    }

    #[inline]
    pub fn declare_builtin_type(&mut self, builtin_data_type: BuiltinDataType) -> HighTypeId {
        self.declare_data_type(HighTypeDeclaration::Builtin(builtin_data_type))
    }

    #[must_use]
    pub fn get_type_id(&self, name: &str) -> Option<HighTypeId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.types.get(name))
            .copied()
    }

    #[must_use]
    pub fn get_data_type_id_semantic_analysis(
        &mut self,
        name_span: Span,
        name: &str,
    ) -> Option<HighTypeId> {
        let Some(id) = self.get_type_id(name) else {
            return self.add_error(
                name_span,
                SemanticAnalysisError::UnknownType(name.to_owned()),
            );
        };

        Some(id)
    }

    #[must_use]
    pub fn get_type(&self, id: HighTypeId) -> &HighTypeDeclaration {
        &self.high_environment.types[id.0]
    }

    #[inline]
    #[must_use]
    pub fn get_struct_type(&self, id: StructId) -> &StructDeclaration {
        self.environment.get_struct(id)
    }
}
