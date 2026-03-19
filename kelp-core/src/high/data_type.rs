use std::{collections::BTreeMap, str::FromStr};

use crate::{
    high::snbt_string::SNBTString,
    middle::{
        data_type::DataType as MiddleDataType,
        data_type_declaration::{BuiltinDataType, DataTypeDeclarationKind},
    },
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
    span::Span,
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DataType {
    Named(Span, String, Vec<Self>),
    TypedCompound(BTreeMap<SNBTString, Self>),
    Reference(Box<Self>),
    Tuple(Vec<Self>),
    Unit,
    Inferred,
}

impl DataType {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        context_generic_names: Option<&Vec<String>>,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleDataType> {
        Some(match self {
            Self::Named(name_span, name, generic_types) => {
                let actual_generic_count = generic_types.len();

                if let Ok(builtin_type) = BuiltinDataType::from_str(&name) {
                    let expected_generic_count = builtin_type.generic_count();

                    if actual_generic_count != expected_generic_count {
                        return ctx.add_invalid_generics(
                            name_span,
                            builtin_type.to_string(),
                            expected_generic_count,
                            actual_generic_count,
                        );
                    }

                    let generic_types = generic_types
                        .into_iter()
                        .map(|generic_type| {
                            generic_type.perform_semantic_analysis(context_generic_names, ctx)
                        })
                        .collect_option_all()?;

                    return builtin_type.to_data_type(generic_types);
                }

                match ctx.get_data_type(&name) {
                    Some(None) => return None,
                    Some(Some(declaration)) => {
                        let expected_generic_count = declaration.generic_count();

                        if actual_generic_count != expected_generic_count {
                            return ctx.add_invalid_generics(
                                name_span,
                                declaration.name(),
                                expected_generic_count,
                                actual_generic_count,
                            );
                        }

                        let generic_types = generic_types
                            .into_iter()
                            .map(|generic_type| {
                                generic_type.perform_semantic_analysis(context_generic_names, ctx)
                            })
                            .collect_option_all::<Vec<_>>()?;

                        match declaration {
                            DataTypeDeclarationKind::Alias { alias, .. } => alias,
                            DataTypeDeclarationKind::Struct { name, .. } => {
                                MiddleDataType::Struct(name, generic_types)
                            }
                            DataTypeDeclarationKind::Builtin(kind) => {
                                kind.to_data_type(generic_types).unwrap()
                            }
                        }
                    }
                    None => {
                        if context_generic_names.is_none_or(|context_generic_names| {
                            !context_generic_names.contains(&name)
                        }) {
                            return ctx.add_info(SemanticAnalysisInfo {
                                span: name_span,
                                kind: SemanticAnalysisInfoKind::Error(
                                    SemanticAnalysisError::UnknownType(name.clone()),
                                ),
                            });
                        }

                        MiddleDataType::Generic(name)
                    }
                }
            }
            Self::Unit => MiddleDataType::Unit,
            Self::Inferred => MiddleDataType::Inferred,
            Self::Tuple(data_types) => {
                let data_types = data_types
                    .into_iter()
                    .map(|data_type| {
                        data_type.perform_semantic_analysis(context_generic_names, ctx)
                    })
                    .collect_option_all()?;

                MiddleDataType::Tuple(data_types)
            }
            Self::Reference(data_type) => {
                let data_type = data_type.perform_semantic_analysis(context_generic_names, ctx)?;

                MiddleDataType::Reference(Box::new(data_type))
            }
            Self::TypedCompound(compound) => {
                let compound = compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = value.perform_semantic_analysis(context_generic_names, ctx)?;

                        Some((key.snbt_string, value))
                    })
                    .collect_option_all()?;

                MiddleDataType::TypedCompound(compound)
            }
        })
    }
}
