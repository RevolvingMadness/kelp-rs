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
    trait_ext::OptionIterExt,
};

fn substitute_data_type(
    data_type: &DataTypeKind,
    substitutions: &BTreeMap<String, DataTypeKind>,
) -> DataTypeKind {
    match data_type {
        DataTypeKind::List(inner) => {
            DataTypeKind::List(Box::new(substitute_data_type(inner, substitutions)))
        }
        DataTypeKind::Compound(inner) => {
            DataTypeKind::Compound(Box::new(substitute_data_type(inner, substitutions)))
        }
        DataTypeKind::Data(inner) => {
            DataTypeKind::Data(Box::new(substitute_data_type(inner, substitutions)))
        }
        DataTypeKind::Reference(inner) => {
            DataTypeKind::Reference(Box::new(substitute_data_type(inner, substitutions)))
        }
        DataTypeKind::Tuple(inner) => DataTypeKind::Tuple(
            inner
                .iter()
                .map(|t| substitute_data_type(t, substitutions))
                .collect(),
        ),
        DataTypeKind::TypedCompound(inner) => DataTypeKind::TypedCompound(
            inner
                .iter()
                .map(|(k, v)| (k.clone(), substitute_data_type(v, substitutions)))
                .collect(),
        ),
        DataTypeKind::Generic(name) => substitutions
            .get(name)
            .cloned()
            .unwrap_or_else(|| data_type.clone()),
        _ => data_type.clone(),
    }
}

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
        supports_variable_type_scope: &mut dyn SupportsVariableTypeScope,
        generics: Option<&Vec<String>>,
    ) -> DataTypeKind {
        match self {
            HighDataTypeKind::Named(_, name, inner_generics) => match name.as_str() {
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
                    if inner_generics.is_empty() {
                        DataTypeKind::List(Box::new(DataTypeKind::SNBT))
                    } else if inner_generics.len() == 1
                        && let Some(first) = inner_generics.first()
                    {
                        DataTypeKind::List(Box::new(
                            first.kind.resolve(supports_variable_type_scope, generics),
                        ))
                    } else {
                        unreachable!()
                    }
                }
                "compound" => {
                    if inner_generics.is_empty() {
                        DataTypeKind::Compound(Box::new(DataTypeKind::SNBT))
                    } else if inner_generics.len() == 1
                        && let Some(first) = inner_generics.first()
                    {
                        DataTypeKind::Compound(Box::new(
                            first.kind.resolve(supports_variable_type_scope, generics),
                        ))
                    } else {
                        unreachable!()
                    }
                }
                "data" => {
                    if inner_generics.is_empty() {
                        DataTypeKind::Data(Box::new(DataTypeKind::SNBT))
                    } else if inner_generics.len() == 1
                        && let Some(first) = inner_generics.first()
                    {
                        DataTypeKind::Data(Box::new(
                            first.kind.resolve(supports_variable_type_scope, generics),
                        ))
                    } else {
                        unreachable!()
                    }
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
                        })
                        .unwrap();

                    match data_type {
                        DataTypeDeclarationKind::Alias {
                            generics: alias_generics,
                            alias,
                            ..
                        } => {
                            if inner_generics.len()
                                != alias_generics
                                    .as_ref()
                                    .map(|generics| generics.len())
                                    .unwrap_or(0)
                            {
                                todo!()
                            }

                            if let Some(alias_generics) = alias_generics {
                                let mut substitutions = BTreeMap::new();
                                for (param, arg) in alias_generics.iter().zip(inner_generics.iter())
                                {
                                    substitutions.insert(
                                        param.clone(),
                                        arg.kind.resolve(supports_variable_type_scope, generics),
                                    );
                                }

                                substitute_data_type(&alias, &substitutions)
                            } else {
                                alias
                            }
                        }
                        DataTypeDeclarationKind::Generic(name) => DataTypeKind::Generic(name),
                    }
                }
            },
            HighDataTypeKind::Unit => DataTypeKind::Unit,
            HighDataTypeKind::Tuple(data_types) => DataTypeKind::Tuple(
                data_types
                    .iter()
                    .map(|data_type| {
                        data_type
                            .kind
                            .resolve(supports_variable_type_scope, generics)
                    })
                    .collect(),
            ),
            HighDataTypeKind::Reference(data_type) => DataTypeKind::Reference(Box::new(
                data_type
                    .kind
                    .resolve(supports_variable_type_scope, generics),
            )),
            HighDataTypeKind::TypedCompound(compound) => DataTypeKind::TypedCompound(
                compound
                    .iter()
                    .map(|(key, value)| {
                        (
                            key.snbt_string.clone(),
                            value.kind.resolve(supports_variable_type_scope, generics),
                        )
                    })
                    .collect(),
            ),
        }
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
            HighDataTypeKind::Named(span, name, inner_generics) => match name.as_str() {
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
                    } else if let Some(first) = inner_generics.first() {
                        return first.perform_semantic_analysis(generics, ctx);
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
                    } else if let Some(first) = inner_generics.first() {
                        return first.perform_semantic_analysis(generics, ctx);
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
                    } else if let Some(first) = inner_generics.first() {
                        return first.perform_semantic_analysis(generics, ctx);
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
                            generics,
                            inner_generics,
                        );
                    }
                    None => {
                        if !generics.is_some_and(|generics| generics.contains(name)) {
                            return ctx.add_info(SemanticAnalysisInfo {
                                span: *span,
                                kind: SemanticAnalysisInfoKind::Error(
                                    SemanticAnalysisError::UnknownType(name.clone()),
                                ),
                            });
                        }
                    }
                },
            },
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
