use std::{collections::BTreeMap, str::FromStr};

use minecraft_command_types::impl_has_macro_false;
use parser_rs::parser_range::ParserRange;

use crate::{
    data_type::{BuiltinDataTypeKind, DataTypeKind},
    datapack::DataTypeDeclarationKind,
    expression::supports_variable_type_scope::SupportsVariableTypeScope,
    high::snbt_string::HighSNBTString,
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
    trait_ext::OptionUnitIterExt,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HighDataTypeKind {
    Named(ParserRange, String, Vec<HighDataType>),
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
                if let Ok(builtin_type) = BuiltinDataTypeKind::from_str(name) {
                    return builtin_type.to_data_type(
                        supports_variable_type_scope,
                        generic_names,
                        generic_types,
                    );
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
                        if let Some(alias_generics) = alias_generic_names {
                            let mut substitutions: BTreeMap<String, DataTypeKind> = BTreeMap::new();

                            for (alias_generic_name, generic_type) in
                                alias_generics.iter().zip(generic_types.iter())
                            {
                                substitutions.insert(
                                    alias_generic_name.clone(),
                                    generic_type
                                        .kind
                                        .resolve(supports_variable_type_scope, generic_names)?,
                                );
                            }

                            alias.substitute(&substitutions)?
                        } else {
                            alias
                        }
                    }
                    DataTypeDeclarationKind::Struct { name, .. } => {
                        let resolved_generics = generic_types
                            .iter()
                            .map(|generic| {
                                generic
                                    .kind
                                    .resolve(supports_variable_type_scope, generic_names)
                            })
                            .collect::<Option<Vec<_>>>()?;
                        DataTypeKind::Struct(name, resolved_generics)
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
    pub span: ParserRange,
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
            HighDataTypeKind::Named(span, name, generic_types) => {
                generic_types
                    .iter()
                    .map(|generic_type| {
                        generic_type.perform_semantic_analysis(context_generics, ctx)
                    })
                    .all_some()?;

                let number_of_generics = generic_types.len();

                let resolved_generic_types: Vec<DataTypeKind> = generic_types
                    .iter()
                    .map(|generic| generic.kind.resolve(ctx, context_generics).unwrap())
                    .collect();

                if let Ok(builtin_type) = BuiltinDataTypeKind::from_str(name) {
                    let actual_generic_count = resolved_generic_types.len();
                    let expected_generic_count = builtin_type.generic_count();

                    if actual_generic_count != expected_generic_count {
                        return ctx.add_invalid_generics(
                            self.span,
                            builtin_type.to_string(),
                            expected_generic_count,
                            actual_generic_count,
                        );
                    }

                    return Some(());
                }

                match ctx.get_data_type(name) {
                    Some(result) => {
                        return result?.perform_semantic_analysis(
                            ctx,
                            self.span,
                            number_of_generics,
                            &resolved_generic_types,
                        );
                    }
                    None => {
                        if context_generics
                            .is_none_or(|context_generics| !context_generics.contains(name))
                        {
                            return ctx.add_info(SemanticAnalysisInfo {
                                span: *span,
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
