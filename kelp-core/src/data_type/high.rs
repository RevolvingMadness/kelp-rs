use std::collections::BTreeMap;

use minecraft_command_types::impl_has_macro_false;
use parser_rs::parser_range::ParserRange;

use crate::{
    data_type::DataTypeKind,
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

fn resolve_named_type(
    supports_variable_type_scope: &mut impl SupportsVariableTypeScope,
    name: &String,
    generics: Option<&Vec<String>>,
    inner_generics: &[HighDataType],
) -> Option<DataTypeKind> {
    Some(match name.as_str() {
        "boolean" => {
            debug_assert!(inner_generics.is_empty());

            DataTypeKind::Boolean
        }
        "byte" => {
            debug_assert!(inner_generics.is_empty());

            DataTypeKind::Byte
        }
        "short" => {
            debug_assert!(inner_generics.is_empty());

            DataTypeKind::Short
        }
        "integer" | "int" => {
            debug_assert!(inner_generics.is_empty());

            DataTypeKind::Integer
        }
        "long" => {
            debug_assert!(inner_generics.is_empty());

            DataTypeKind::Long
        }
        "float" => {
            debug_assert!(inner_generics.is_empty());

            DataTypeKind::Float
        }
        "double" => {
            debug_assert!(inner_generics.is_empty());

            DataTypeKind::Double
        }
        "string" | "str" => {
            debug_assert!(inner_generics.is_empty());

            DataTypeKind::String
        }
        "score" => {
            debug_assert!(inner_generics.is_empty());

            DataTypeKind::Score
        }
        "list" => {
            debug_assert!(inner_generics.len() == 1);

            let first = inner_generics.first().unwrap();

            DataTypeKind::List(Box::new(
                first.kind.resolve(supports_variable_type_scope, generics)?,
            ))
        }
        "compound" => {
            debug_assert!(inner_generics.len() == 1);

            let first = inner_generics.first().unwrap();

            DataTypeKind::Compound(Box::new(
                first.kind.resolve(supports_variable_type_scope, generics)?,
            ))
        }
        "data" => {
            debug_assert!(inner_generics.len() == 1);

            let first = inner_generics.first().unwrap();

            DataTypeKind::Data(Box::new(
                first.kind.resolve(supports_variable_type_scope, generics)?,
            ))
        }
        "snbt" => DataTypeKind::SNBT,
        _ => {
            let data_type = supports_variable_type_scope
                .get_data_type(name)
                .unwrap_or_else(|| {
                    if let Some(generics) = generics
                        && generics.contains(name)
                    {
                        Some(DataTypeDeclarationKind::Generic(name.clone()))
                    } else {
                        None
                    }
                })?;

            match data_type {
                DataTypeDeclarationKind::Alias {
                    generics: alias_generics,
                    alias,
                    ..
                } => {
                    if let Some(alias_generics) = alias_generics {
                        let mut substitutions: BTreeMap<String, DataTypeKind> = BTreeMap::new();

                        for (alias_generic_name, generic_type) in
                            alias_generics.iter().zip(inner_generics.iter())
                        {
                            substitutions.insert(
                                alias_generic_name.clone(),
                                generic_type
                                    .kind
                                    .resolve(supports_variable_type_scope, generics)?,
                            );
                        }

                        alias.substitute(&substitutions)?
                    } else {
                        alias
                    }
                }
                DataTypeDeclarationKind::Struct { name, .. } => {
                    let resolved_generics = inner_generics
                        .iter()
                        .map(|generic| generic.kind.resolve(supports_variable_type_scope, generics))
                        .collect::<Option<Vec<_>>>()?;
                    DataTypeKind::Struct(name, resolved_generics)
                }
                DataTypeDeclarationKind::Generic(name) => DataTypeKind::Generic(name),
                DataTypeDeclarationKind::Builtin(_) => unreachable!(),
            }
        }
    })
}

impl HighDataTypeKind {
    pub fn resolve(
        &self,
        supports_variable_type_scope: &mut impl SupportsVariableTypeScope,
        generics: Option<&Vec<String>>,
    ) -> Option<DataTypeKind> {
        Some(match self {
            HighDataTypeKind::Named(_, name, inner_generics) => {
                resolve_named_type(supports_variable_type_scope, name, generics, inner_generics)?
            }
            HighDataTypeKind::Unit => DataTypeKind::Unit,
            HighDataTypeKind::Tuple(data_types) => DataTypeKind::Tuple(
                data_types
                    .iter()
                    .map(|data_type| {
                        data_type
                            .kind
                            .resolve(supports_variable_type_scope, generics)
                    })
                    .collect::<Option<_>>()?,
            ),
            HighDataTypeKind::Reference(data_type) => DataTypeKind::Reference(Box::new(
                data_type
                    .kind
                    .resolve(supports_variable_type_scope, generics)?,
            )),
            HighDataTypeKind::TypedCompound(compound) => DataTypeKind::TypedCompound(
                compound
                    .iter()
                    .map(|(key, value)| {
                        value
                            .kind
                            .resolve(supports_variable_type_scope, generics)
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
        generics: Option<&Vec<String>>,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<()> {
        match &self.kind {
            HighDataTypeKind::Named(span, name, inner_generics) => {
                inner_generics
                    .iter()
                    .map(|generic| generic.perform_semantic_analysis(generics, ctx))
                    .all_some()?;

                let inner_generics_len = inner_generics.len();

                let inner_generics: Vec<DataTypeKind> = inner_generics
                    .iter()
                    .map(|generic| generic.kind.resolve(ctx, generics).unwrap())
                    .collect();

                match name.as_str() {
                    "boolean" | "bool" => {
                        if !inner_generics.is_empty() {
                            return ctx.add_invalid_generics(
                                self.span,
                                "boolean".to_string(),
                                0,
                                inner_generics.len(),
                            );
                        }
                    }
                    "byte" => {
                        if !inner_generics.is_empty() {
                            return ctx.add_invalid_generics(
                                self.span,
                                "byte".to_string(),
                                0,
                                inner_generics.len(),
                            );
                        }
                    }
                    "short" => {
                        if !inner_generics.is_empty() {
                            return ctx.add_invalid_generics(
                                self.span,
                                "short".to_string(),
                                0,
                                inner_generics.len(),
                            );
                        }
                    }
                    "integer" | "int" => {
                        if !inner_generics.is_empty() {
                            return ctx.add_invalid_generics(
                                self.span,
                                "integer".to_string(),
                                0,
                                inner_generics.len(),
                            );
                        }
                    }
                    "long" => {
                        if !inner_generics.is_empty() {
                            return ctx.add_invalid_generics(
                                self.span,
                                "long".to_string(),
                                0,
                                inner_generics.len(),
                            );
                        }
                    }
                    "float" => {
                        if !inner_generics.is_empty() {
                            return ctx.add_invalid_generics(
                                self.span,
                                "float".to_string(),
                                0,
                                inner_generics.len(),
                            );
                        }
                    }
                    "double" => {
                        if !inner_generics.is_empty() {
                            return ctx.add_invalid_generics(
                                self.span,
                                "double".to_string(),
                                0,
                                inner_generics.len(),
                            );
                        }
                    }
                    "string" | "str" => {
                        if !inner_generics.is_empty() {
                            return ctx.add_invalid_generics(
                                self.span,
                                "string".to_string(),
                                0,
                                inner_generics.len(),
                            );
                        }
                    }
                    "unit" => {
                        // TODO "did you mean ()?""
                        if ctx.get_variable(name).is_none() {
                            return ctx.add_info(SemanticAnalysisInfo {
                                span: *span,
                                kind: SemanticAnalysisInfoKind::Error(
                                    SemanticAnalysisError::UnknownType(name.clone()),
                                ),
                            });
                        }
                    }
                    "score" => {
                        if !inner_generics.is_empty() {
                            return ctx.add_invalid_generics(
                                self.span,
                                "score".to_string(),
                                0,
                                inner_generics.len(),
                            );
                        }
                    }
                    "list" => {
                        if inner_generics.len() != 1 {
                            return ctx.add_invalid_generics(
                                self.span,
                                "list".to_string(),
                                1,
                                inner_generics.len(),
                            );
                        }
                    }
                    "compound" => {
                        if inner_generics.len() != 1 {
                            return ctx.add_invalid_generics(
                                self.span,
                                "compound".to_string(),
                                1,
                                inner_generics.len(),
                            );
                        }
                    }
                    "data" => {
                        if inner_generics.len() != 1 {
                            return ctx.add_invalid_generics(
                                self.span,
                                "data".to_string(),
                                1,
                                inner_generics.len(),
                            );
                        }
                    }
                    "snbt" => {
                        if !inner_generics.is_empty() {
                            return ctx.add_invalid_generics(
                                self.span,
                                "snbt".to_string(),
                                0,
                                inner_generics.len(),
                            );
                        }
                    }
                    _ => match ctx.get_data_type(name) {
                        Some(result) => {
                            return result?.perform_semantic_analysis(
                                ctx,
                                self.span,
                                inner_generics_len,
                                &inner_generics,
                            );
                        }
                        None => {
                            if generics.is_none_or(|generics| !generics.contains(name)) {
                                return ctx.add_info(SemanticAnalysisInfo {
                                    span: *span,
                                    kind: SemanticAnalysisInfoKind::Error(
                                        SemanticAnalysisError::UnknownType(name.clone()),
                                    ),
                                });
                            }
                        }
                    },
                }
            }
            HighDataTypeKind::Unit => (),
            HighDataTypeKind::Tuple(data_types) => data_types
                .iter()
                .map(|data_type| data_type.perform_semantic_analysis(generics, ctx))
                .all_some()?,
            HighDataTypeKind::Reference(data_type) => {
                return data_type.perform_semantic_analysis(generics, ctx);
            }
            HighDataTypeKind::TypedCompound(compound) => compound
                .values()
                .map(|value| value.perform_semantic_analysis(generics, ctx))
                .all_some()?,
        }

        Some(())
    }
}
