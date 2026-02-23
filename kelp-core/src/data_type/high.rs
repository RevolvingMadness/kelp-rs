use std::{collections::BTreeMap, str::FromStr};

use minecraft_command_types::impl_has_macro_false;

use crate::{
    data_type::{BuiltinDataTypeKind, DataTypeKind},
    datapack::DataTypeDeclarationKind,
    expression::supports_variable_type_scope::SupportsVariableTypeScope,
    high::snbt_string::HighSNBTString,
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
    span::Span,
    trait_ext::OptionUnitIterExt,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HighDataTypeKind {
    Named(Span, String, Vec<HighDataType>),
    TypedCompound(BTreeMap<HighSNBTString, HighDataType>),
    Reference(Box<HighDataType>),
    Tuple(Vec<HighDataType>),
    Unit,
}

impl HighDataTypeKind {
    pub fn resolve(
        &self,
        supports_variable_type_scope: &impl SupportsVariableTypeScope,
        generic_names: Option<&Vec<String>>,
    ) -> Option<DataTypeKind> {
        Some(match self {
            HighDataTypeKind::Named(_, name, generic_types) => {
                let generic_types = generic_types
                    .iter()
                    .map(|generic_type| {
                        generic_type
                            .kind
                            .resolve(supports_variable_type_scope, generic_names)
                    })
                    .collect::<Option<Vec<_>>>()?;

                if let Ok(builtin_type) = BuiltinDataTypeKind::from_str(name) {
                    return builtin_type.to_data_type(generic_types);
                }

                let data_type = supports_variable_type_scope
                    .get_data_type(name)
                    .unwrap_or_else(|| {
                        if let Some(generics) = generic_names
                            && generics.contains(name)
                        {
                            Some(DataTypeDeclarationKind::Generic(name.clone()))
                        } else {
                            None
                        }
                    })?;

                match data_type {
                    DataTypeDeclarationKind::Alias {
                        generics: alias_generic_names,
                        alias,
                        ..
                    } => {
                        let mut substitutions: BTreeMap<String, DataTypeKind> = BTreeMap::new();

                        for (alias_generic_name, generic_type) in alias_generic_names
                            .into_iter()
                            .zip(generic_types.into_iter())
                        {
                            substitutions.insert(alias_generic_name, generic_type);
                        }

                        alias.substitute(&substitutions)?
                    }
                    DataTypeDeclarationKind::Struct { name, .. } => {
                        DataTypeKind::Struct(name, generic_types, false)
                    }
                    DataTypeDeclarationKind::Generic(name) => DataTypeKind::Generic(name),
                    DataTypeDeclarationKind::Builtin(_) => unreachable!(),
                }
            }
            HighDataTypeKind::Unit => DataTypeKind::Unit,
            HighDataTypeKind::Tuple(data_types) => DataTypeKind::Tuple(
                data_types
                    .iter()
                    .map(|data_type| {
                        data_type
                            .kind
                            .resolve(supports_variable_type_scope, generic_names)
                    })
                    .collect::<Option<_>>()?,
            ),
            HighDataTypeKind::Reference(data_type) => DataTypeKind::Reference(Box::new(
                data_type
                    .kind
                    .resolve(supports_variable_type_scope, generic_names)?,
            )),
            HighDataTypeKind::TypedCompound(compound) => DataTypeKind::TypedCompound(
                compound
                    .iter()
                    .map(|(key, value)| {
                        value
                            .kind
                            .resolve(supports_variable_type_scope, generic_names)
                            .map(|data_type| (key.snbt_string.clone(), data_type))
                    })
                    .collect::<Option<_>>()?,
            ),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighDataType {
    pub span: Span,
    pub kind: HighDataTypeKind,
}

impl_has_macro_false!(HighDataType);

impl HighDataType {
    #[must_use]
    pub fn perform_semantic_analysis(
        &self,
        context_generics: Option<&Vec<String>>,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<()> {
        match &self.kind {
            HighDataTypeKind::Named(name_span, name, generic_types) => {
                let actual_generic_count = generic_types.len();

                if let Ok(builtin_type) = BuiltinDataTypeKind::from_str(name) {
                    let expected_generic_count = builtin_type.generic_count();

                    if actual_generic_count != expected_generic_count {
                        return ctx.add_invalid_generics(
                            *name_span,
                            builtin_type.to_string(),
                            expected_generic_count,
                            actual_generic_count,
                        );
                    }

                    generic_types
                        .iter()
                        .map(|generic_type| {
                            generic_type.perform_semantic_analysis(context_generics, ctx)
                        })
                        .all_some()?;

                    return Some(());
                }

                match ctx.get_data_type(name) {
                    Some(None) => return None,
                    Some(Some(result)) => {
                        let expected_generic_count = result.generic_count();

                        if actual_generic_count != expected_generic_count {
                            return ctx.add_invalid_generics(
                                *name_span,
                                result.name().to_string(),
                                expected_generic_count,
                                actual_generic_count,
                            );
                        }

                        generic_types
                            .iter()
                            .map(|generic_type| {
                                generic_type.perform_semantic_analysis(context_generics, ctx)
                            })
                            .all_some()?;

                        let resolved_generic_types: Vec<DataTypeKind> = generic_types
                            .iter()
                            .map(|generic| generic.kind.resolve(ctx, context_generics).unwrap())
                            .collect();

                        return result.perform_semantic_analysis(
                            ctx,
                            self.span,
                            actual_generic_count,
                            &resolved_generic_types,
                        );
                    }
                    None => {
                        if context_generics
                            .is_none_or(|context_generics| !context_generics.contains(name))
                        {
                            return ctx.add_info(SemanticAnalysisInfo {
                                span: *name_span,
                                kind: SemanticAnalysisInfoKind::Error(
                                    SemanticAnalysisError::UnknownType(name.clone()),
                                ),
                            });
                        }
                    }
                }
            }
            HighDataTypeKind::Unit => {}
            HighDataTypeKind::Tuple(data_types) => data_types
                .iter()
                .map(|data_type| data_type.perform_semantic_analysis(context_generics, ctx))
                .all_some()?,
            HighDataTypeKind::Reference(data_type) => {
                return data_type.perform_semantic_analysis(context_generics, ctx);
            }
            HighDataTypeKind::TypedCompound(compound) => compound
                .values()
                .map(|value| value.perform_semantic_analysis(context_generics, ctx))
                .all_some()?,
        }

        Some(())
    }
}
