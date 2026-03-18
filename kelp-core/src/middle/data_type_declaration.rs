use std::collections::HashMap;

use strum::{Display, EnumString};

use crate::{
    high::supports_variable_type_scope::SupportsVariableTypeScope, middle::data_type::DataTypeKind,
};

#[derive(Display, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum BuiltinDataTypeKind {
    #[strum(serialize = "boolean", serialize = "bool")]
    Boolean,
    Byte,
    Short,
    #[strum(serialize = "integer", serialize = "int")]
    Integer,
    Long,
    Float,
    Double,
    #[strum(serialize = "string", serialize = "str")]
    String,
    #[strum(serialize = "()", serialize = "unit")]
    Unit,
    Score,
    List,
    Compound,
    Data,
    #[strum(
        serialize = "snbt",
        serialize = "nbt",
        serialize = "SNBT",
        serialize = "NBT"
    )]
    SNBT,
}

impl BuiltinDataTypeKind {
    #[must_use]
    pub fn to_data_type(&self, mut generic_types: Vec<DataTypeKind>) -> Option<DataTypeKind> {
        if generic_types.len() != self.generic_count() {
            return None;
        }

        Some(match self {
            Self::Unit => DataTypeKind::Unit,
            Self::Boolean => DataTypeKind::Boolean,
            Self::Byte => DataTypeKind::Byte,
            Self::Short => DataTypeKind::Short,
            Self::Integer => DataTypeKind::Integer,
            Self::Long => DataTypeKind::Long,
            Self::Float => DataTypeKind::Float,
            Self::Double => DataTypeKind::Double,
            Self::String => DataTypeKind::String,
            Self::SNBT => DataTypeKind::SNBT,
            Self::List | Self::Compound | Self::Data | Self::Score => {
                let element_type = generic_types.remove(0);

                match self {
                    Self::List => DataTypeKind::List(Box::new(element_type)),
                    Self::Compound => DataTypeKind::Compound(Box::new(element_type)),
                    Self::Data => DataTypeKind::Data(Box::new(element_type)),
                    Self::Score => DataTypeKind::Score(Box::new(element_type)),
                    _ => unreachable!(),
                }
            }
        })
    }

    #[must_use]
    pub const fn generic_count(&self) -> usize {
        match self {
            Self::Boolean
            | Self::Byte
            | Self::Short
            | Self::Integer
            | Self::Long
            | Self::Float
            | Self::Double
            | Self::String
            | Self::Unit
            | Self::SNBT => 0,
            Self::List | Self::Compound | Self::Data | Self::Score => 1,
        }
    }
}

#[derive(Debug, Clone)]
pub enum DataTypeDeclarationKind {
    Alias {
        name: String,
        generics: Vec<String>,
        alias: DataTypeKind,
    },
    Struct {
        name: String,
        generics: Vec<String>,
        fields: HashMap<String, DataTypeKind>,
    },
    Builtin(BuiltinDataTypeKind),
}

impl DataTypeDeclarationKind {
    pub fn resolve_is_struct(
        self,
        supports_variable_type_scope: &impl SupportsVariableTypeScope,
        generic_types: Vec<DataTypeKind>,
    ) -> Option<bool> {
        Some(matches!(
            self.resolve(supports_variable_type_scope, generic_types)?,
            DataTypeKind::Struct(_, _)
        ))
    }

    #[must_use]
    pub fn name(&self) -> String {
        match self {
            Self::Builtin(builtin_type) => builtin_type.to_string(),
            Self::Alias { name, .. } | Self::Struct { name, .. } => name.clone(),
        }
    }

    #[must_use]
    pub const fn generic_count(&self) -> usize {
        match self {
            Self::Alias {
                generics: generic_names,
                ..
            }
            | Self::Struct {
                generics: generic_names,
                ..
            } => generic_names.len(),
            Self::Builtin(builtin_type) => builtin_type.generic_count(),
        }
    }

    pub fn get_struct_fields(
        &self,
        ctx: &impl SupportsVariableTypeScope,
        generic_types: &[DataTypeKind],
    ) -> Option<HashMap<String, DataTypeKind>> {
        match self {
            Self::Alias {
                generics: generic_names,
                alias,
                ..
            } => {
                let substitutions: HashMap<String, DataTypeKind> = generic_names
                    .iter()
                    .zip(generic_types.iter().cloned())
                    .map(|(k, v)| (k.clone(), v))
                    .collect();

                let resolved_alias = alias.clone().substitute(&substitutions)?;

                if let DataTypeKind::Struct(name, generics) = resolved_alias {
                    let declaration = ctx.get_data_type(&name)??;
                    declaration.get_struct_fields(ctx, &generics)
                } else {
                    None
                }
            }
            Self::Struct {
                fields,
                generics: generic_names,
                ..
            } => {
                let substitutions: HashMap<String, DataTypeKind> = generic_names
                    .iter()
                    .zip(generic_types.iter().cloned())
                    .map(|(k, v)| (k.clone(), v))
                    .collect();

                Some(
                    fields
                        .iter()
                        .map(|(field_name, field_type)| {
                            let substituted = field_type.clone().substitute(&substitutions)?;

                            Some((field_name.clone(), substituted))
                        })
                        .collect::<Option<_>>()?,
                )
            }
            Self::Builtin(_) => None,
        }
    }

    pub fn resolve(
        self,
        supports_variable_type_scope: &impl SupportsVariableTypeScope,
        generic_types: Vec<DataTypeKind>,
    ) -> Option<DataTypeKind> {
        match self {
            Self::Alias {
                generics: generic_names,
                alias,
                ..
            } => {
                let substitutions: HashMap<String, DataTypeKind> =
                    generic_names.into_iter().zip(generic_types).collect();

                let resolved_alias = alias.substitute(&substitutions)?;

                if let DataTypeKind::Struct(name, generics) = resolved_alias {
                    let declaration = supports_variable_type_scope.get_data_type(&name)??;
                    declaration.resolve(supports_variable_type_scope, generics)
                } else {
                    Some(resolved_alias)
                }
            }
            Self::Struct {
                name,
                generics: generic_names,
                ..
            } => {
                let substitutions: HashMap<String, DataTypeKind> = generic_names
                    .into_iter()
                    .zip(generic_types.clone())
                    .collect();

                Some(DataTypeKind::Struct(name, generic_types).substitute(&substitutions)?)
            }
            Self::Builtin(data_type) => data_type.to_data_type(generic_types),
        }
    }
}
