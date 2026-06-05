use std::{
    collections::{BTreeMap, HashMap},
    fmt::{Display, Write},
};

use crate::semantic::environment::{
    SemanticEnvironment,
    r#type::{
        generic::HighGenericId,
        module::HighModuleId,
        r#struct::{
            HighStructId, SemanticStructDeclaration, regular::HighRegularStructId,
            tuple::HighTupleStructId,
        },
    },
    value::function::{HighFunctionId, SemanticFunctionDeclaration},
};
use crate::{
    datapack::Datapack,
    operator::ComparisonOperator,
    parsed::semantic_analysis::{
        SemanticAnalysisContext,
        info::error::{SemanticAnalysisError, ValueKind},
    },
    span::Span,
    visibility::Visibility,
};
use crate::{low::data_type::DataType, semantic::typed_path::SemanticTypedPathSegment};
use crate::{
    low::environment::{
        Environment,
        r#type::r#struct::{RegularStructId, StructId, TupleStructId},
    },
    semantic::environment::value::{SemanticValueDeclaration, SemanticValueDeclarationKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum WrapperType {
    Data,
    Reference,
}

impl WrapperType {
    #[must_use]
    pub fn wrap(self, data_type: SemanticDataType) -> SemanticDataType {
        match self {
            Self::Data => SemanticDataType::Data(Box::new(data_type)),
            Self::Reference => SemanticDataType::Reference(Box::new(data_type)),
        }
    }
}

macro_rules! check_error {
    (bool, $($expr:expr),+) => {
        if false $(|| matches!($expr, SemanticDataType::Error))* {
            return true;
        }
    };

    (result, $($expr:expr),+) => {
        if false $(|| matches!($expr, SemanticDataType::Error))* {
            return Ok(SemanticDataType::Error);
        }
    };

    (tuple, $($expr:expr),+) => {
        if false $(|| matches!($expr, SemanticDataType::Error))* {
            return Some((SemanticDataType::Error, SemanticDataType::Error));
        }
    };

    ($($expr:expr),+) => {
        if false $(|| matches!($expr, SemanticDataType::Error))* {
            return Some(SemanticDataType::Error);
        }
    };
}

#[derive(Debug, Clone)]
pub struct CallInfo {
    pub id: Option<HighFunctionId>,
    pub declaration_span: Option<Span>,
    pub parameter_types: Vec<SemanticDataType>,
    pub return_type: SemanticDataType,
}

#[derive(Debug, Clone)]
pub struct MethodInfo {
    pub id: HighFunctionId,
    pub generic_types: Vec<SemanticDataType>,
    pub return_type: SemanticDataType,
}

pub struct SemanticDataTypeDisplay<'a> {
    pub data_type: &'a SemanticDataType,
    pub semantic_environment: &'a SemanticEnvironment,
}

impl Display for SemanticDataTypeDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.data_type {
            SemanticDataType::Boolean => f.write_str("bool"),
            SemanticDataType::Byte => f.write_str("byte"),
            SemanticDataType::Short => f.write_str("short"),
            SemanticDataType::Integer => f.write_str("integer"),
            SemanticDataType::Long => f.write_str("long"),
            SemanticDataType::Float => f.write_str("float"),
            SemanticDataType::Double => f.write_str("double"),
            SemanticDataType::String => f.write_str("string"),
            SemanticDataType::Unit => f.write_str("()"),
            SemanticDataType::Never => f.write_char('!'),
            SemanticDataType::Score(data_type) => {
                f.write_str("score<")?;
                data_type.display(self.semantic_environment).fmt(f)?;
                f.write_char('>')?;

                Ok(())
            }
            SemanticDataType::List(data_type) => {
                f.write_str("list<")?;
                data_type.display(self.semantic_environment).fmt(f)?;
                f.write_char('>')?;

                Ok(())
            }
            SemanticDataType::TypedCompound(compound) => {
                f.write_char('{')?;

                if !compound.is_empty() {
                    f.write_char(' ')?;
                }

                for (i, (key, value_data_type)) in compound.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    write!(f, "{}: ", key)?;

                    value_data_type.display(self.semantic_environment).fmt(f)?;
                }

                if !compound.is_empty() {
                    f.write_char(' ')?;
                }

                f.write_char('}')?;

                Ok(())
            }
            SemanticDataType::Compound(data_type) => {
                f.write_str("compound<")?;
                data_type.display(self.semantic_environment).fmt(f)?;
                f.write_char('>')?;

                Ok(())
            }
            SemanticDataType::Data(data_type) => {
                f.write_str("data<")?;
                data_type.display(self.semantic_environment).fmt(f)?;
                f.write_char('>')?;

                Ok(())
            }
            SemanticDataType::Reference(data_type) => {
                f.write_char('&')?;
                data_type.display(self.semantic_environment).fmt(f)?;

                Ok(())
            }
            SemanticDataType::Tuple(data_types) => {
                f.write_char('(')?;

                for (i, data_type) in data_types.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    data_type.display(self.semantic_environment).fmt(f)?;
                }

                f.write_char(')')?;

                Ok(())
            }
            SemanticDataType::Generic(id) => {
                let (_, _, declaration) = self.semantic_environment.get_generic(*id);

                declaration.name.fmt(f)
            }
            SemanticDataType::Function(id, generic_types) => {
                let declaration = self.semantic_environment.get_function_declaration(*id);

                write!(f, "fn {}", declaration.name())?;

                if !generic_types.is_empty() {
                    f.write_str("<")?;

                    for (i, data_type) in generic_types.iter().enumerate() {
                        if i != 0 {
                            f.write_str(", ")?;
                        }

                        data_type.display(self.semantic_environment).fmt(f)?;
                    }

                    f.write_str(">")?;
                }

                f.write_char('(')?;

                for (i, data_type) in declaration.parameter_types().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    data_type.display(self.semantic_environment).fmt(f)?;
                }

                write!(
                    f,
                    ") -> {}",
                    declaration.return_type().display(self.semantic_environment)
                )?;

                Ok(())
            }
            SemanticDataType::Struct(id, generic_types) => {
                // Maybe display full path?

                let declaration = self.semantic_environment.get_struct_declaration(*id);

                f.write_str(declaration.name())?;

                if !generic_types.is_empty() {
                    f.write_str("<")?;

                    for (i, generic_type) in generic_types.iter().enumerate() {
                        if i != 0 {
                            f.write_str(", ")?;
                        }

                        generic_type.display(self.semantic_environment).fmt(f)?;
                    }

                    f.write_str(">")?;
                }

                Ok(())
            }
            SemanticDataType::Inferred => f.write_char('_'),
            SemanticDataType::InferredInteger => f.write_str("{integer}"),
            SemanticDataType::InferredFloat => f.write_str("{float}"),
            SemanticDataType::ResourceLocation => f.write_str("resource_location"),
            SemanticDataType::EntitySelector => f.write_str("entity_selector"),
            SemanticDataType::Coordinates => f.write_str("coordinates"),
            SemanticDataType::Error => f.write_str("{error}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SemanticDataType {
    Boolean,
    Byte,
    Short,
    Integer,
    Long,
    Float,
    Double,
    String,
    Unit,
    Never,
    Score(Box<Self>),
    List(Box<Self>),
    TypedCompound(BTreeMap<String, Self>),
    Compound(Box<Self>),
    Data(Box<Self>),
    Reference(Box<Self>),
    Tuple(Vec<Self>),
    Generic(HighGenericId),
    Function(HighFunctionId, Vec<Self>),
    Struct(HighStructId, Vec<Self>),
    Inferred,
    InferredInteger,
    InferredFloat,
    ResourceLocation,
    EntitySelector,
    Coordinates,
    Error,
}

impl SemanticDataType {
    #[must_use]
    pub fn unwrap(&self) -> (Vec<WrapperType>, &Self) {
        let mut wrappers = Vec::new();
        let mut current = self;

        loop {
            match current {
                Self::Data(inner) => {
                    wrappers.push(WrapperType::Data);

                    current = inner;
                }
                Self::Reference(inner) => {
                    wrappers.push(WrapperType::Reference);

                    current = inner;
                }
                _ => break,
            }
        }

        (wrappers, current)
    }

    #[must_use]
    pub fn wrap(mut self, wrappers: &[WrapperType]) -> Self {
        for wrapper in wrappers.iter().rev() {
            self = wrapper.wrap(self);
        }

        self
    }

    #[must_use]
    #[allow(clippy::too_many_arguments)]
    pub fn inner_resolve_regular_struct(
        datapack: &mut Datapack,
        id: HighRegularStructId,
        module_path: Vec<HighModuleId>,
        visibility: Visibility,
        name: String,
        generic_ids: &[HighGenericId],
        field_types: HashMap<String, Self>,
        generic_types: Vec<Self>,
    ) -> RegularStructId {
        let field_types = field_types
            .into_iter()
            .map(|(name, data_type)| {
                let data_type = data_type
                    .substitute_generics(generic_ids, &generic_types)
                    .resolve(datapack)
                    .unwrap();

                (name, data_type)
            })
            .collect();

        let generic_types = generic_types
            .into_iter()
            .map(|data_type| data_type.resolve(datapack).unwrap())
            .collect();

        datapack.declare_monomorphized_regular_struct(
            module_path,
            visibility,
            id.into(),
            name,
            generic_types,
            field_types,
        )
    }

    #[must_use]
    #[allow(clippy::too_many_arguments)]
    pub fn inner_monomorphize_tuple_struct(
        datapack: &mut Datapack,
        id: HighTupleStructId,
        module_path: Vec<HighModuleId>,
        visibility: Visibility,
        name: String,
        generic_ids: &[HighGenericId],
        field_types: Vec<Self>,
        generic_types: Vec<Self>,
    ) -> TupleStructId {
        let field_types = field_types
            .into_iter()
            .map(|data_type| {
                data_type
                    .substitute_generics(generic_ids, &generic_types)
                    .resolve(datapack)
                    .unwrap()
            })
            .collect();

        let generic_types = generic_types
            .into_iter()
            .map(|data_type| data_type.resolve(datapack).unwrap())
            .collect();

        datapack.declare_monomorphized_tuple_struct(
            module_path,
            visibility,
            id.into(),
            name,
            generic_types,
            field_types,
        )
    }

    #[must_use]
    fn monomorphize_struct(
        datapack: &mut Datapack,
        id: HighStructId,
        generic_types: Vec<Self>,
    ) -> StructId {
        let (module_path, visibility, declaration) = {
            let (module_path, visiblity, declaration) =
                datapack.semantic_environment.get_struct(id);

            (module_path.to_vec(), visiblity, declaration.clone())
        };

        match declaration {
            SemanticStructDeclaration::Struct(declaration) => {
                let id = HighRegularStructId(id.0);

                Self::inner_resolve_regular_struct(
                    datapack,
                    id,
                    module_path,
                    visibility,
                    declaration.name,
                    &declaration.generic_ids,
                    declaration.field_types,
                    generic_types,
                )
                .into()
            }
            SemanticStructDeclaration::Tuple(declaration) => {
                let id = HighTupleStructId(id.0);

                Self::inner_monomorphize_tuple_struct(
                    datapack,
                    id,
                    module_path,
                    visibility,
                    declaration.name,
                    &declaration.generic_ids,
                    declaration.field_types,
                    generic_types,
                )
                .into()
            }
        }
    }

    #[must_use]
    pub fn monomorphize_regular_struct(
        datapack: &mut Datapack,
        id: HighRegularStructId,
        generic_types: Vec<Self>,
    ) -> RegularStructId {
        let (module_path, visibility, declaration) = {
            let (module_path, visiblity, declaration) =
                datapack.semantic_environment.get_regular_struct(id);

            (module_path.to_vec(), visiblity, declaration.clone())
        };

        Self::inner_resolve_regular_struct(
            datapack,
            id,
            module_path,
            visibility,
            declaration.name.clone(),
            &declaration.generic_ids,
            declaration.field_types.clone(),
            generic_types,
        )
    }

    #[must_use]
    pub fn monomorphize_tuple_struct(
        datapack: &mut Datapack,
        id: HighTupleStructId,
        generic_types: Vec<Self>,
    ) -> TupleStructId {
        let (module_path, visibility, declaration) = {
            let (module_path, visiblity, declaration) =
                datapack.semantic_environment.get_tuple_struct(id);

            (module_path.to_vec(), visiblity, declaration.clone())
        };

        Self::inner_monomorphize_tuple_struct(
            datapack,
            id,
            module_path,
            visibility,
            declaration.name.clone(),
            &declaration.generic_ids,
            declaration.field_types.clone(),
            generic_types,
        )
    }

    #[must_use]
    pub fn substitute_generics(
        self,
        generic_ids: &[HighGenericId],
        generic_types: &[Self],
    ) -> Self {
        match self {
            Self::Boolean => Self::Boolean,
            Self::Byte => Self::Byte,
            Self::Short => Self::Short,
            Self::Integer => Self::Integer,
            Self::Long => Self::Long,
            Self::Float => Self::Float,
            Self::Double => Self::Double,
            Self::String => Self::String,
            Self::Unit => Self::Unit,
            Self::Never => Self::Never,
            Self::Score(data_type) => {
                let data_type = data_type.substitute_generics(generic_ids, generic_types);

                Self::Score(Box::new(data_type))
            }
            Self::List(data_type) => {
                let data_type = data_type.substitute_generics(generic_ids, generic_types);

                Self::List(Box::new(data_type))
            }
            Self::TypedCompound(compound) => {
                let compound = compound
                    .into_iter()
                    .map(|(key, data_type)| {
                        let data_type = data_type.substitute_generics(generic_ids, generic_types);

                        (key, data_type)
                    })
                    .collect();

                Self::TypedCompound(compound)
            }
            Self::Compound(data_type) => {
                let data_type = data_type.substitute_generics(generic_ids, generic_types);

                Self::Compound(Box::new(data_type))
            }
            Self::Data(data_type) => {
                let data_type = data_type.substitute_generics(generic_ids, generic_types);

                Self::Data(Box::new(data_type))
            }
            Self::Reference(data_type) => {
                let data_type = data_type.substitute_generics(generic_ids, generic_types);

                Self::Reference(Box::new(data_type))
            }
            Self::Tuple(data_types) => {
                let data_types = data_types
                    .into_iter()
                    .map(|data_type| data_type.substitute_generics(generic_ids, generic_types))
                    .collect();

                Self::Tuple(data_types)
            }
            Self::Generic(target_id) => {
                if let Some(index) = generic_ids
                    .iter()
                    .position(|generic_id| *generic_id == target_id)
                {
                    generic_types[index].clone()
                } else {
                    self
                }
            }
            Self::Function(id, data_types) => {
                let data_types = data_types
                    .into_iter()
                    .map(|data_type| data_type.substitute_generics(generic_ids, generic_types))
                    .collect();

                Self::Function(id, data_types)
            }
            Self::Struct(id, data_types) => {
                let data_types = data_types
                    .into_iter()
                    .map(|data_type| data_type.substitute_generics(generic_ids, generic_types))
                    .collect();

                Self::Struct(id, data_types)
            }
            Self::Inferred => Self::Inferred,
            Self::InferredInteger => Self::InferredInteger,
            Self::InferredFloat => Self::InferredFloat,
            Self::ResourceLocation => Self::ResourceLocation,
            Self::EntitySelector => Self::EntitySelector,
            Self::Coordinates => Self::Coordinates,
            Self::Error => Self::Error,
        }
    }

    #[allow(clippy::result_unit_err)]
    pub fn get_method(
        &self,
        ctx: &mut SemanticAnalysisContext,
        segment: &SemanticTypedPathSegment,
    ) -> Option<MethodInfo> {
        if *self == Self::Error {
            return None;
        }

        let (_, base_type) = self.unwrap();

        Some(match base_type {
            Self::Struct(id, struct_generic_types) => {
                let implementations = ctx.semantic_environment.get_implementations(*id)?;

                let implementation = implementations.iter().find(|implementation| {
                    if let Self::Struct(impl_id, generic_types) = implementation.get_target_type() {
                        impl_id == id && struct_generic_types == generic_types
                    } else {
                        false
                    }
                })?;

                let method_id = implementation.get_value(&segment.name)?;

                let SemanticValueDeclaration {
                    kind: SemanticValueDeclarationKind::Function(declaration),
                    ..
                } = ctx.semantic_environment.get_value(method_id)
                else {
                    return ctx.add_error(SemanticAnalysisError::MethodNotFound {
                        type_span: segment.name_span,
                        type_: self.clone(),
                        method_name: segment.name.clone(),
                    });
                };

                if !declaration.is_method() {
                    // TODO: SemanticAnalysisError::NotAMethod

                    return ctx.add_error(SemanticAnalysisError::MethodNotFound {
                        type_span: segment.name_span,
                        type_: self.clone(),
                        method_name: segment.name.clone(),
                    });
                }

                let expected_generic_count = declaration.declared_generic_count();
                let actual_generic_count = segment.generic_types.len();

                if actual_generic_count != expected_generic_count {
                    return ctx.add_error(SemanticAnalysisError::InvalidGenerics {
                        type_name_span: segment.name_span,
                        item_kind: ValueKind::Function.into(),
                        declaration_span: declaration.name_span(),
                        expected: expected_generic_count,
                        actual: actual_generic_count,
                    });
                }

                let method_id = HighFunctionId(method_id.0);

                let mut all_generic_types = struct_generic_types.clone();
                all_generic_types.extend(segment.generic_types.iter().cloned());

                let return_type = declaration
                    .return_type()
                    .clone()
                    .substitute_generics(declaration.generic_ids(), &all_generic_types);

                MethodInfo {
                    id: method_id,
                    generic_types: all_generic_types,
                    return_type,
                }
            }
            _ => {
                return ctx.add_error(SemanticAnalysisError::MethodNotFound {
                    type_span: segment.name_span,
                    type_: self.clone(),
                    method_name: segment.name.clone(),
                });
            }
        })
    }

    #[must_use]
    pub fn resolve(self, datapack: &mut Datapack) -> Option<DataType> {
        Some(match self {
            Self::Boolean => DataType::Boolean,
            Self::Byte => DataType::Byte,
            Self::Short => DataType::Short,
            Self::Integer => DataType::Integer,
            Self::Long => DataType::Long,
            Self::Float => DataType::Float,
            Self::Double => DataType::Double,
            Self::String => DataType::String,
            Self::Unit => DataType::Unit,
            Self::Never => DataType::Never,
            Self::Score(data_type) => {
                let data_type = data_type.resolve(datapack)?;

                DataType::Score(Box::new(data_type))
            }
            Self::List(data_type) => {
                let data_type = data_type.resolve(datapack)?;

                DataType::List(Box::new(data_type))
            }
            Self::TypedCompound(compound) => {
                let compound = compound
                    .into_iter()
                    .map(|(key, data_type)| {
                        let data_type = data_type.resolve(datapack)?;

                        Some((key, data_type))
                    })
                    .collect::<Option<_>>()?;

                DataType::TypedCompound(compound)
            }
            Self::Compound(data_type) => {
                let data_type = data_type.resolve(datapack)?;

                DataType::Compound(Box::new(data_type))
            }
            Self::Data(data_type) => {
                let data_type = data_type.resolve(datapack)?;

                DataType::Data(Box::new(data_type))
            }
            Self::Reference(data_type) => {
                let data_type = data_type.resolve(datapack)?;

                DataType::Reference(Box::new(data_type))
            }
            Self::Tuple(data_types) => {
                let data_types = data_types
                    .into_iter()
                    .map(|data_type| data_type.resolve(datapack))
                    .collect::<Option<_>>()?;

                DataType::Tuple(data_types)
            }
            Self::Generic(id) => return datapack.resolve_generic(id),
            Self::Function(id, generic_types) => {
                let id = datapack.get_monomorphized_function_id(id, &generic_types);

                DataType::Function(id)
            }
            Self::Struct(id, generic_types) => {
                let id = Self::monomorphize_struct(datapack, id, generic_types);

                DataType::Struct(id)
            }
            Self::Inferred => DataType::Inferred,
            Self::InferredInteger => DataType::InferredInteger,
            Self::InferredFloat => DataType::InferredFloat,
            Self::ResourceLocation => DataType::ResourceLocation,
            Self::EntitySelector => DataType::EntitySelector,
            Self::Coordinates => DataType::Coordinates,
            Self::Error => return None,
        })
    }

    #[must_use]
    pub fn get_call_info(
        &self,
        semantic_environment: &SemanticEnvironment,
    ) -> Option<Option<CallInfo>> {
        Some(Some(match self {
            Self::Error => return None,

            Self::Function(id, generic_types) => {
                let declaration = semantic_environment.get_function_declaration(*id);

                match declaration {
                    SemanticFunctionDeclaration::Regular(declaration) => CallInfo {
                        id: Some(*id),
                        declaration_span: Some(declaration.name_span),
                        parameter_types: declaration
                            .parameters
                            .iter()
                            .map(|(_, data_type)| {
                                data_type
                                    .clone()
                                    .substitute_generics(&declaration.generic_ids, generic_types)
                            })
                            .collect(),
                        return_type: declaration
                            .return_type
                            .clone()
                            .substitute_generics(&declaration.generic_ids, generic_types),
                    },
                    SemanticFunctionDeclaration::Builtin(declaration) => CallInfo {
                        id: None,
                        declaration_span: None,
                        parameter_types: declaration
                            .parameters
                            .iter()
                            .map(|data_type| {
                                data_type
                                    .clone()
                                    .substitute_generics(&declaration.generic_ids, generic_types)
                            })
                            .collect(),
                        return_type: declaration
                            .return_type
                            .clone()
                            .substitute_generics(&declaration.generic_ids, generic_types),
                    },
                }
            }
            Self::ResourceLocation => CallInfo {
                id: None,
                declaration_span: None,
                return_type: Self::Integer,
                parameter_types: Vec::new(),
            },
            _ => return Some(None),
        }))
    }

    #[must_use]
    pub fn reduce(self, other: &Self) -> Option<Self> {
        check_error!(self, other);

        if self.equals(other) {
            if self == Self::Inferred {
                return Some(other.clone());
            }

            if *other == Self::Inferred {
                return Some(self);
            }

            if self == Self::InferredInteger {
                return Some(other.clone());
            }

            if *other == Self::InferredInteger {
                return Some(self);
            }

            if self == Self::InferredFloat {
                return Some(other.clone());
            }

            if *other == Self::InferredFloat {
                return Some(self);
            }

            return Some(self);
        }

        match (self, other) {
            (Self::Inferred, other) => Some(other.clone()),
            (this, Self::Inferred) => Some(this),

            (Self::InferredInteger, other) if other.is_integer_like() => Some(other.clone()),
            (this, Self::InferredInteger) if this.is_integer_like() => Some(this),

            (Self::InferredFloat, other) if other.is_float_like() => Some(other.clone()),
            (this, Self::InferredFloat) if this.is_float_like() => Some(this),

            (Self::Data(inner), other) => inner
                .reduce(other)
                .map_or(Some(Self::Inferred), |reduced_inner| {
                    Some(Self::Data(Box::new(reduced_inner)))
                }),
            (this, Self::Data(inner)) => this
                .reduce(inner)
                .map_or(Some(Self::Inferred), |reduced_inner| {
                    Some(Self::Data(Box::new(reduced_inner)))
                }),

            (Self::Score(inner), other) => inner
                .reduce(other)
                .map_or(Some(Self::Inferred), |reduced_inner| {
                    Some(Self::Score(Box::new(reduced_inner)))
                }),
            (this, Self::Score(inner)) => this
                .reduce(inner)
                .map_or(Some(Self::Inferred), |reduced_inner| {
                    Some(Self::Score(Box::new(reduced_inner)))
                }),

            (Self::Reference(a), Self::Reference(b)) => {
                Some(Self::Reference(Box::new(a.reduce(b)?)))
            }

            (Self::List(a), Self::List(b)) => Some(Self::List(Box::new(a.reduce(b)?))),

            (Self::Compound(a), Self::Compound(b)) => Some(Self::Compound(Box::new(a.reduce(b)?))),

            (Self::TypedCompound(map), Self::Compound(inner)) => {
                if map.values().all(|v| v.equals(inner)) {
                    Some(Self::Compound(inner.clone()))
                } else {
                    None
                }
            }

            (Self::Compound(inner), Self::TypedCompound(map)) => {
                if map.values().all(|v| v.equals(&inner)) {
                    Some(Self::Compound(inner))
                } else {
                    None
                }
            }

            (Self::TypedCompound(a_map), Self::TypedCompound(b_map)) => {
                if a_map.len() != b_map.len() {
                    return None;
                }
                let mut new_map = BTreeMap::new();
                for (key, a_val) in a_map {
                    let b_val = b_map.get(&key)?;
                    let reduced = a_val.reduce(b_val)?;
                    new_map.insert(key, reduced);
                }
                Some(Self::TypedCompound(new_map))
            }

            (Self::Tuple(a_vec), Self::Tuple(b_vec)) => {
                if a_vec.len() != b_vec.len() {
                    return None;
                }
                let mut new_vec = Vec::new();
                for (a, b) in a_vec.into_iter().zip(b_vec.iter()) {
                    new_vec.push(a.reduce(b)?);
                }
                Some(Self::Tuple(new_vec))
            }

            (Self::Struct(a_id, a_generic_types), Self::Struct(b_id, b_generic_types))
                if a_id == *b_id && a_generic_types == *b_generic_types =>
            {
                Some(Self::Struct(a_id, a_generic_types))
            }

            _ => None,
        }
    }

    #[must_use]
    pub const fn display<'a>(
        &'a self,
        semantic_environment: &'a SemanticEnvironment,
    ) -> SemanticDataTypeDisplay<'a> {
        SemanticDataTypeDisplay {
            data_type: self,
            semantic_environment,
        }
    }
}

impl SemanticDataType {
    #[must_use]
    pub const fn is_integer_like(&self) -> bool {
        self.is_restricted_integer_like() || matches!(self, Self::Long)
    }

    #[must_use]
    pub const fn is_restricted_integer_like(&self) -> bool {
        matches!(
            self,
            Self::Byte | Self::Short | Self::Integer | Self::InferredInteger
        )
    }

    #[must_use]
    pub const fn is_float_like(&self) -> bool {
        matches!(self, Self::Float | Self::Double | Self::InferredFloat)
    }

    #[must_use]
    pub const fn is_numeric(&self) -> bool {
        self.is_integer_like() || self.is_float_like()
    }

    #[must_use]
    pub fn is_compiletime(&self) -> Option<bool> {
        Some(match self {
            Self::Error => return None,

            Self::Score(..) | Self::Data(..) => false,
            Self::Reference(data_type) => return data_type.is_compiletime(),

            Self::Boolean
            | Self::Byte
            | Self::Short
            | Self::Integer
            | Self::Long
            | Self::Float
            | Self::Double
            | Self::String
            | Self::Never
            | Self::Unit
            | Self::List(..)
            | Self::TypedCompound(..)
            | Self::Compound(..)
            | Self::Tuple(..)
            | Self::Generic(..)
            | Self::Struct(..)
            | Self::Function(..)
            | Self::ResourceLocation
            | Self::EntitySelector
            | Self::Coordinates
            | Self::Inferred // TODO
            | Self::InferredInteger
            | Self::InferredFloat => true,
        })
    }

    pub fn get_iterable_type(self) -> Result<Self, Self> {
        check_error!(result, self);

        Ok(match self {
            Self::Reference(inner) => inner.get_iterable_type()?,

            Self::List(data_type) => *data_type,
            Self::String => self,

            Self::Data(data_type) => data_type.get_iterable_type()?,

            _ => return Err(self),
        })
    }

    #[must_use]
    pub fn get_iterable_type_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        span: Span,
    ) -> Option<Self> {
        match self.get_iterable_type() {
            Ok(data_type) => Some(data_type),
            Err(data_type) => ctx.add_error(SemanticAnalysisError::CannotIterateType {
                type_span: span,
                data_type,
            }),
        }
    }

    #[must_use]
    pub const fn is_runtime(&self) -> bool {
        match self {
            Self::Reference(data_type) => data_type.is_runtime(),

            Self::Score(..) | Self::Data(..) => true,

            _ => false,
        }
    }

    #[must_use]
    pub fn can_cast_to(&self, data_type: &Self) -> bool {
        check_error!(bool, self, data_type);

        match (self, data_type) {
            (Self::Score(self_type), Self::Score(data_type)) => self_type.can_cast_to(data_type),
            (Self::Score(self_type), Self::Data(data_type)) => self_type.can_cast_to(data_type),
            (Self::Data(self_type), Self::Data(data_type)) => self_type.can_cast_to(data_type),
            (Self::Data(self_type), Self::Score(data_type)) => self_type.can_cast_to(data_type),

            (Self::List(self_type), Self::List(data_type)) if self_type.can_cast_to(data_type) => {
                true
            }
            (Self::Compound(self_type), Self::Compound(data_type))
                if self_type.can_cast_to(data_type) =>
            {
                true
            }

            (Self::Inferred, _)
            | (
                Self::InferredInteger
                | Self::InferredFloat
                | Self::Byte
                | Self::Short
                | Self::Integer
                | Self::Long
                | Self::Float
                | Self::Double,
                Self::Byte | Self::Short | Self::Integer | Self::Long | Self::Float | Self::Double,
            ) => true,

            _ => self == data_type,
        }
    }

    #[must_use]
    pub fn can_be_assigned_to_score(&self) -> bool {
        match self {
            Self::Score(..) => true,
            Self::Data(data_type) => data_type.can_be_assigned_to_score(),
            Self::Reference(data_type) => data_type.can_be_assigned_to_score(),
            _ => self.is_score_compatible(),
        }
    }

    #[must_use]
    pub const fn is_score_compatible(&self) -> bool {
        matches!(
            self,
            Self::Error
                | Self::Boolean
                | Self::Byte
                | Self::Short
                | Self::Integer
                | Self::InferredInteger
                | Self::Score(..)
        )
    }

    #[must_use]
    pub fn get_data_type(&self, semantic_environment: &SemanticEnvironment) -> Option<Self> {
        check_error!(self);

        Some(match self {
            Self::Error => Self::Error,

            Self::Boolean => Self::Boolean,
            Self::Byte => Self::Byte,
            Self::Short => Self::Short,
            Self::InferredInteger | Self::Integer => Self::Integer,
            Self::Long => Self::Long,
            Self::InferredFloat | Self::Float => Self::Float,
            Self::Double => Self::Double,
            Self::String => Self::String,
            Self::Unit => Self::Unit,
            Self::Never => Self::Never,
            Self::Score(data_type) => data_type.get_data_type(semantic_environment)?,
            Self::List(data_type) => {
                let data_type = data_type.get_data_type(semantic_environment)?;

                Self::List(Box::new(data_type))
            }
            Self::TypedCompound(compound) => {
                let compound = compound
                    .iter()
                    .map(|(key, data_type)| {
                        let data_type = data_type.get_data_type(semantic_environment)?;

                        Some((key.clone(), data_type))
                    })
                    .collect::<Option<_>>()?;

                Self::TypedCompound(compound)
            }
            Self::Compound(data_type) => {
                let data_type = data_type.get_data_type(semantic_environment)?;

                Self::Compound(Box::new(data_type))
            }
            Self::Data(data_type) => data_type.get_data_type(semantic_environment)?,
            Self::Reference(data_type) => {
                let data_type = data_type.get_data_type(semantic_environment)?;

                Self::Reference(Box::new(data_type))
            }
            Self::Tuple(data_types) => {
                let data_types = data_types
                    .iter()
                    .map(|data_type| data_type.get_data_type(semantic_environment))
                    .collect::<Option<_>>()?;

                Self::Tuple(data_types)
            }
            Self::Struct(id, generic_types) => {
                let declaration = semantic_environment.get_struct_declaration(*id);

                let generic_types = generic_types
                    .iter()
                    .map(|data_type| data_type.get_data_type(semantic_environment))
                    .collect::<Option<Vec<_>>>()?;

                match declaration {
                    SemanticStructDeclaration::Struct(declaration) => {
                        for data_type in declaration.field_types.values() {
                            data_type
                                .clone()
                                .substitute_generics(&declaration.generic_ids, &generic_types)
                                .get_data_type(semantic_environment)?;
                        }
                    }
                    SemanticStructDeclaration::Tuple(declaration) => {
                        for data_type in &declaration.field_types {
                            data_type
                                .clone()
                                .substitute_generics(&declaration.generic_ids, &generic_types)
                                .get_data_type(semantic_environment)?;
                        }
                    }
                }

                Self::Struct(*id, generic_types)
            }
            _ => return None,
        })
    }

    #[must_use]
    pub fn has_fields(&self) -> bool {
        check_error!(bool, self);

        match self {
            Self::TypedCompound(..) => true,
            Self::Compound(..) => true,
            Self::Data(..) => true,
            Self::Reference(data_type) => data_type.has_fields(),
            Self::Tuple(..) => true,
            Self::Struct(..) => true,
            _ => false,
        }
    }

    fn raw_get_arithmetic_result(&self, other: &Self) -> Option<Self> {
        check_error!(self, other);

        if self.is_numeric() && other.is_numeric() {
            return Some(match (self, other) {
                (Self::InferredInteger, other) | (other, Self::InferredInteger) => {
                    if matches!(other, Self::InferredFloat) {
                        Self::Float
                    } else {
                        other.clone()
                    }
                }

                (Self::InferredFloat, other) | (other, Self::InferredFloat) => {
                    if matches!(other, Self::Double) {
                        Self::Double
                    } else {
                        Self::Float
                    }
                }

                (Self::Double, _) | (_, Self::Double) => Self::Double,
                (Self::Float, _) | (_, Self::Float) => Self::Float,
                (Self::Long, _) | (_, Self::Long) => Self::Long,
                (Self::Integer, _) | (_, Self::Integer) => Self::Integer,
                (Self::Short, _) | (_, Self::Short) => Self::Short,
                (Self::Byte, _) | (_, Self::Byte) => Self::Byte,

                _ => return None,
            });
        }

        match (self, other) {
            (Self::Score(inner), other) | (other, Self::Score(inner))
                if other.can_be_assigned_to_score() && inner.can_be_assigned_to_score() =>
            {
                Some(Self::Score(Box::new(Self::Integer)))
            }
            (Self::Data(inner), other) | (other, Self::Data(inner))
                if other.can_be_assigned_to_score() && inner.can_be_assigned_to_score() =>
            {
                Some(Self::Score(Box::new(Self::Integer)))
            }
            _ => None,
        }
    }

    #[must_use]
    pub fn get_arithmetic_result(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Reference(self_), other) | (other, Self::Reference(self_)) => {
                self_.raw_get_arithmetic_result(other)
            }

            _ => self.raw_get_arithmetic_result(other),
        }
    }

    #[must_use]
    pub fn can_perform_assignment(&self, other: &Self) -> bool {
        check_error!(bool, self, other);

        match (self, other) {
            (Self::Byte | Self::InferredInteger, Self::Byte | Self::InferredInteger)
            | (Self::Short | Self::InferredInteger, Self::Short | Self::InferredInteger)
            | (Self::Integer | Self::InferredInteger, Self::Integer | Self::InferredInteger)
            | (Self::Long | Self::InferredInteger, Self::Long | Self::InferredInteger) => true,
            (Self::Data(inner) | Self::Score(inner), other)
            | (other, Self::Data(inner) | Self::Score(inner)) => {
                inner.can_perform_assignment(other)
            }
            _ => false,
        }
    }

    #[must_use]
    pub fn can_perform_augmented_assignment(&self, other: &Self) -> bool {
        check_error!(bool, self, other);

        match (self, other) {
            (Self::Byte | Self::InferredInteger, Self::Byte | Self::InferredInteger)
            | (Self::Short | Self::InferredInteger, Self::Short | Self::InferredInteger)
            | (Self::Integer | Self::InferredInteger, Self::Integer | Self::InferredInteger)
            | (Self::Long | Self::InferredInteger, Self::Long | Self::InferredInteger) => true,
            (Self::Data(inner) | Self::Score(inner), other)
            | (other, Self::Data(inner) | Self::Score(inner)) => {
                inner.can_perform_augmented_assignment(other)
            }
            _ => false,
        }
    }

    #[must_use]
    fn raw_can_perform_comparison(
        &self,
        environment: &Environment,
        operator: ComparisonOperator,
        other: &Self,
    ) -> bool {
        check_error!(bool, self, other);

        if self.is_numeric() && other.is_numeric() {
            return true;
        }

        match (self, other) {
            (Self::Score(inner), other_type) | (other_type, Self::Score(inner))
                if inner.can_be_assigned_to_score() && other_type.can_be_assigned_to_score() =>
            {
                inner.can_perform_comparison(environment, operator, other_type)
            }
            (Self::Data(inner), other) | (other, Self::Data(inner))
                if (operator == ComparisonOperator::EqualTo
                    || operator == ComparisonOperator::NotEqualTo)
                    || (inner.can_be_assigned_to_score() && other.can_be_assigned_to_score()) =>
            {
                inner.can_perform_comparison(environment, operator, other)
            }
            _ => false,
        }
    }

    #[must_use]
    pub fn can_perform_comparison(
        &self,
        environment: &Environment,
        operator: ComparisonOperator,
        other: &Self,
    ) -> bool {
        if (operator == ComparisonOperator::EqualTo || operator == ComparisonOperator::NotEqualTo)
            && self.equals(other)
        {
            return true;
        }

        match (self, other) {
            (Self::Reference(data_type), other) | (other, Self::Reference(data_type)) => {
                data_type.raw_can_perform_comparison(environment, operator, other)
            }

            _ => self.raw_can_perform_comparison(environment, operator, other),
        }
    }

    fn get_index_result(&self) -> Option<(Self, Self)> {
        check_error!(tuple, self);

        Some(match self {
            Self::List(data_type) => (*data_type.clone(), Self::Integer),
            Self::Data(data_type) => {
                let (index_result_type, preferred_index_type) = data_type
                    .get_index_result()
                    .unwrap_or((Self::Inferred, Self::Inferred));

                (
                    Self::Data(Box::new(index_result_type)),
                    preferred_index_type,
                )
            }
            Self::Reference(data_type) => data_type.get_index_result()?,

            _ => return None,
        })
    }

    #[must_use]
    pub fn get_index_result_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        span: Span,
        index_type: &Self,
    ) -> Option<Self> {
        let Some((index_result_type, preferred_index_type)) = self.get_index_result() else {
            return ctx.add_error(SemanticAnalysisError::CannotBeIndexed {
                type_span: span,
                data_type: self.clone(),
            });
        };

        let (_, index_type) = index_type.unwrap();
        let (_, preferred_index_type) = preferred_index_type.unwrap();

        if !index_type.equals(preferred_index_type) {
            return ctx.add_error(SemanticAnalysisError::TypeCannotBeIndexedByType {
                target_span: span,
                target: self.clone(),
                index: index_type.clone(),
            });
        }

        Some(index_result_type)
    }

    fn get_field_result(
        &self,
        semantic_environment: &SemanticEnvironment,
        field: &str,
    ) -> Option<Self> {
        check_error!(self);

        Some(match self {
            Self::Reference(inner) => inner.get_field_result(semantic_environment, field)?,

            Self::Struct(id, generic_types) => {
                let declaration = semantic_environment.get_struct_declaration(*id);

                let field_type = declaration.get_field(field)?;

                field_type
                    .clone()
                    .substitute_generics(declaration.generic_ids(), generic_types)
            }

            Self::TypedCompound(compound) => compound.iter().find_map(|(key, data_type)| {
                if key == field {
                    Some(data_type.clone())
                } else {
                    None
                }
            })?,

            Self::Compound(data_type) => *data_type.clone(),

            Self::Data(data_type) => Self::Data(Box::new(
                data_type
                    .get_field_result(semantic_environment, field)
                    .unwrap_or(Self::Inferred),
            )),

            Self::Tuple(items) => {
                let index = field.parse::<usize>().ok()?;

                items.get(index)?.clone()
            }

            _ => return None,
        })
    }

    #[must_use]
    pub fn get_field_result_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        span: Span,
        field: &str,
    ) -> Option<Self> {
        if !self.has_fields() {
            return ctx.add_error(SemanticAnalysisError::TypeDoesntHaveFields {
                type_span: span,
                data_type: self.clone(),
            });
        }

        match self.get_field_result(&ctx.semantic_environment, field) {
            Some(result) => Some(result),
            None => ctx.add_error(SemanticAnalysisError::TypeDoesntHaveField {
                field_span: span,
                data_type: self.clone(),
                field: field.to_owned(),
            }),
        }
    }

    #[must_use]
    pub fn get_dereferenced_assignment_result(self) -> Option<Self> {
        check_error!(self);

        Some(match self {
            Self::Reference(data_type) | Self::Score(data_type) | Self::Data(data_type) => {
                *data_type
            }
            _ => return None,
        })
    }

    pub fn get_dereferenced_result(self) -> Result<Self, Self> {
        check_error!(result, self);

        Ok(match self {
            Self::Reference(data_type) => *data_type,

            Self::Score(..) | Self::Data(..) => self,

            _ => return Err(self),
        })
    }

    #[must_use]
    pub fn get_dereferenced_result_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        span: Span,
    ) -> Option<Self> {
        match self.get_dereferenced_result() {
            Ok(result) => Some(result),
            Err(self_) => ctx.add_error(SemanticAnalysisError::CannotBeDereferenced {
                type_span: span,
                data_type: self_,
            }),
        }
    }

    #[must_use]
    pub fn get_negation_result(&self) -> Option<Self> {
        check_error!(self);

        Some(match self {
            Self::Reference(self_) => return self_.get_negation_result(),

            Self::Byte => Self::Byte,
            Self::Short => Self::Short,
            Self::Integer | Self::InferredInteger => Self::Integer,
            Self::Long => Self::Long,
            Self::Score(inner_type) => Self::Score(inner_type.clone()),
            _ => return None,
        })
    }

    #[must_use]
    pub fn get_inverted_result(&self) -> Option<Self> {
        check_error!(self);

        Some(match self {
            Self::Reference(self_) => return self_.get_inverted_result(),

            Self::Boolean => Self::Boolean,

            _ => return None,
        })
    }

    #[must_use]
    pub fn equals(&self, other: &Self) -> bool {
        check_error!(bool, self, other);

        match (self, other) {
            (Self::Inferred, _) | (_, Self::Inferred) => true,

            (_, Self::Never) => true,

            (Self::InferredInteger, rhs) | (rhs, Self::InferredInteger)
                if rhs.is_integer_like() =>
            {
                true
            }

            (Self::InferredFloat, rhs) | (rhs, Self::InferredFloat) if rhs.is_float_like() => true,

            (Self::List(data_types), Self::List(other_data_types))
            | (Self::Compound(data_types), Self::Compound(other_data_types))
            | (Self::Score(data_types), Self::Score(other_data_types))
            | (Self::Data(data_types), Self::Data(other_data_types)) => {
                data_types.equals(other_data_types)
            }

            (Self::Tuple(data_types), Self::Tuple(other_data_types)) => {
                data_types.len() == other_data_types.len()
                    && data_types
                        .iter()
                        .zip(other_data_types)
                        .all(|(s, o)| s.equals(o))
            }

            (Self::TypedCompound(data_types), Self::TypedCompound(other_data_types)) => {
                other_data_types.iter().all(|(key, data_type)| {
                    data_types.get(key).is_some_and(|sv| sv.equals(data_type))
                })
            }

            (Self::Compound(inner), Self::TypedCompound(map))
            | (Self::TypedCompound(map), Self::Compound(inner)) => {
                map.values().all(|v| v.equals(inner))
            }

            _ => self == other,
        }
    }
}

impl SemanticDataType {
    #[must_use]
    pub fn assert_runtime_value_mutation_in_runtime_loop(
        &self,
        ctx: &mut SemanticAnalysisContext,
        span: Span,
    ) -> Option<()> {
        if ctx.loop_depth != 0 && self.is_compiletime()? {
            return ctx.add_error(
                SemanticAnalysisError::CompiletimeValueMutationInRuntimeLoop { value_span: span },
            );
        }

        Some(())
    }

    #[must_use]
    pub fn assert_equals(
        &self,
        ctx: &mut SemanticAnalysisContext,
        span: Span,
        other: &Self,
    ) -> Option<()> {
        if self.equals(other) {
            return Some(());
        }

        ctx.add_error(SemanticAnalysisError::MismatchedTypes {
            span,
            expected: other.clone(),
            actual: self.clone(),
        })
    }

    #[must_use]
    pub fn assert_score_compatible(
        &self,
        ctx: &mut SemanticAnalysisContext,
        span: Span,
    ) -> Option<()> {
        if self.can_be_assigned_to_score() {
            return Some(());
        }

        ctx.add_error(SemanticAnalysisError::TypeIsNotScoreCompatible {
            type_span: span,
            data_type: self.clone(),
        })
    }

    #[must_use]
    pub fn assert_data_compatible(
        &self,
        ctx: &mut SemanticAnalysisContext,
        span: Span,
    ) -> Option<()> {
        if self.get_data_type(&ctx.semantic_environment).is_some() {
            return Some(());
        }

        ctx.add_error(SemanticAnalysisError::TypeIsNotDataCompatible {
            type_span: span,
            data_type: self.clone(),
        })
    }

    #[must_use]
    pub fn assert_condition(&self, ctx: &mut SemanticAnalysisContext, span: Span) -> Option<()> {
        if *self == Self::Boolean {
            return Some(());
        }

        ctx.add_error(SemanticAnalysisError::MismatchedTypes {
            span,
            expected: Self::Boolean,
            actual: self.clone(),
        })
    }
}
