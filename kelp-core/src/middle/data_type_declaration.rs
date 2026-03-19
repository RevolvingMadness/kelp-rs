use std::collections::HashMap;

use crate::{
    builtin_data_type::BuiltinDataType,
    high::supports_variable_type_scope::SupportsVariableTypeScope, middle::data_type::DataType,
};

#[derive(Debug, Clone)]
pub struct AliasDeclaration {
    pub name: String,
    pub generic_names: Vec<String>,
    pub alias: DataType,
}

#[derive(Debug, Clone)]
pub struct StructDeclaration {
    pub name: String,
    pub generic_names: Vec<String>,
    pub field_types: HashMap<String, DataType>,
}

#[derive(Debug, Clone)]
pub enum TypeDeclaration {
    Struct(StructDeclaration),
    Alias(AliasDeclaration),
    Builtin(BuiltinDataType),
}

impl TypeDeclaration {
    pub fn resolve_is_struct(
        self,
        supports_variable_type_scope: &impl SupportsVariableTypeScope,
        generic_types: Vec<DataType>,
    ) -> Option<bool> {
        Some(matches!(
            self.resolve(supports_variable_type_scope, generic_types)?,
            DataType::Struct(_, _)
        ))
    }

    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Struct(declaration) => &declaration.name,
            Self::Alias(declaration) => &declaration.name,
            Self::Builtin(data_type) => data_type.name(),
        }
    }

    #[must_use]
    pub const fn generic_count(&self) -> usize {
        match self {
            Self::Struct(declaration) => declaration.generic_names.len(),
            Self::Alias(declaration) => declaration.generic_names.len(),
            Self::Builtin(builtin_type) => builtin_type.generic_count(),
        }
    }

    pub fn get_struct_fields(
        &self,
        ctx: &impl SupportsVariableTypeScope,
        generic_types: &[DataType],
    ) -> Option<HashMap<String, DataType>> {
        match self {
            Self::Alias(declaration) => {
                let substitutions: HashMap<String, DataType> = declaration
                    .generic_names
                    .iter()
                    .cloned()
                    .zip(generic_types.iter().cloned())
                    .collect();

                let alias = declaration.alias.clone().substitute(&substitutions)?;

                if let DataType::Struct(name, generics) = alias {
                    let declaration = ctx.get_data_type(&name)??;
                    declaration.get_struct_fields(ctx, &generics)
                } else {
                    None
                }
            }
            Self::Struct(declaration) => {
                let substitutions: HashMap<String, DataType> = declaration
                    .generic_names
                    .iter()
                    .cloned()
                    .zip(generic_types.iter().cloned())
                    .collect();

                Some(
                    declaration
                        .field_types
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
        generic_types: Vec<DataType>,
    ) -> Option<DataType> {
        match self {
            Self::Alias(declaration) => {
                let substitutions: HashMap<String, DataType> = declaration
                    .generic_names
                    .into_iter()
                    .zip(generic_types)
                    .collect();

                let resolved_alias = declaration.alias.substitute(&substitutions)?;

                if let DataType::Struct(name, generics) = resolved_alias {
                    let declaration = supports_variable_type_scope.get_data_type(&name)??;
                    declaration.resolve(supports_variable_type_scope, generics)
                } else {
                    Some(resolved_alias)
                }
            }
            Self::Struct(declaration) => {
                let substitutions: HashMap<String, DataType> = declaration
                    .generic_names
                    .into_iter()
                    .zip(generic_types.clone())
                    .collect();

                Some(DataType::Struct(declaration.name, generic_types).substitute(&substitutions)?)
            }
            Self::Builtin(data_type) => data_type.to_data_type(generic_types),
        }
    }
}
