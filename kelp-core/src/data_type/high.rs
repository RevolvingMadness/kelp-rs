use std::collections::BTreeMap;

use minecraft_command_types::impl_has_macro_false;
use parser_rs::parser_range::ParserRange;

use crate::{
    data_type::{DataTypeKind, GenericDataTypeKind},
    high::snbt_string::HighSNBTString,
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
    trait_ext::OptionIterExt,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HighDataTypeKind {
    Named(ParserRange, String, Vec<HighDataType>),
    TypedCompound(BTreeMap<HighSNBTString, HighDataType>),
    Reference(Box<HighDataType>),
    Unit,
}

impl HighDataTypeKind {
    pub fn resolve(&self) -> DataTypeKind {
        match self {
            HighDataTypeKind::Named(_, name, generics) => match name.as_str() {
                "byte" => {
                    if !generics.is_empty() {
                        unreachable!()
                    }

                    DataTypeKind::Byte
                }
                "short" => {
                    if !generics.is_empty() {
                        unreachable!()
                    }

                    DataTypeKind::Short
                }
                "integer" | "int" => {
                    if !generics.is_empty() {
                        unreachable!()
                    }

                    DataTypeKind::Integer
                }
                "long" => {
                    if !generics.is_empty() {
                        unreachable!()
                    }

                    DataTypeKind::Long
                }
                "float" => {
                    if !generics.is_empty() {
                        unreachable!()
                    }

                    DataTypeKind::Float
                }
                "double" => {
                    if !generics.is_empty() {
                        unreachable!()
                    }

                    DataTypeKind::Double
                }
                "string" | "str" => {
                    if !generics.is_empty() {
                        unreachable!()
                    }

                    DataTypeKind::String
                }
                "score" => {
                    if !generics.is_empty() {
                        unreachable!()
                    }

                    DataTypeKind::Score
                }
                "list" => {
                    if generics.is_empty() {
                        DataTypeKind::List(Box::new(DataTypeKind::Any))
                    } else if generics.len() == 1
                        && let Some(first) = generics.first()
                    {
                        DataTypeKind::List(Box::new(first.kind.resolve()))
                    } else {
                        unreachable!()
                    }
                }
                "compound" => {
                    if generics.is_empty() {
                        DataTypeKind::Compound(Box::new(DataTypeKind::Any))
                    } else if generics.len() == 1
                        && let Some(first) = generics.first()
                    {
                        DataTypeKind::Compound(Box::new(first.kind.resolve()))
                    } else {
                        unreachable!()
                    }
                }
                "data" => {
                    if generics.is_empty() {
                        DataTypeKind::Data(Box::new(DataTypeKind::Any))
                    } else if generics.len() == 1
                        && let Some(first) = generics.first()
                    {
                        DataTypeKind::Data(Box::new(first.kind.resolve()))
                    } else {
                        unreachable!()
                    }
                }
                "any" => DataTypeKind::Any,
                _ => DataTypeKind::Custom(
                    name.clone(),
                    generics
                        .iter()
                        .map(|generic| generic.kind.resolve())
                        .collect(),
                ),
            },
            HighDataTypeKind::Unit => DataTypeKind::Unit,
            HighDataTypeKind::Reference(data_type) => {
                DataTypeKind::Reference(Box::new(data_type.kind.resolve()))
            }
            HighDataTypeKind::TypedCompound(compound) => DataTypeKind::TypedCompound(
                compound
                    .iter()
                    .map(|(key, value)| (key.snbt_string.clone(), value.kind.resolve()))
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
    pub fn perform_semantic_analysis(&self, ctx: &mut SemanticAnalysisContext) -> Option<()> {
        match &self.kind {
            HighDataTypeKind::Named(span, name, generics) => match name.as_str() {
                "byte" => {
                    if !generics.is_empty() {
                        return ctx.add_invalid_generics(
                            self.span,
                            GenericDataTypeKind::Byte,
                            0,
                            generics.len(),
                        );
                    }
                }
                "short" => {
                    if !generics.is_empty() {
                        return ctx.add_invalid_generics(
                            self.span,
                            GenericDataTypeKind::Short,
                            0,
                            generics.len(),
                        );
                    }
                }
                "integer" | "int" => {
                    if !generics.is_empty() {
                        return ctx.add_invalid_generics(
                            self.span,
                            GenericDataTypeKind::Integer,
                            0,
                            generics.len(),
                        );
                    }
                }
                "long" => {
                    if !generics.is_empty() {
                        return ctx.add_invalid_generics(
                            self.span,
                            GenericDataTypeKind::Long,
                            0,
                            generics.len(),
                        );
                    }
                }
                "float" => {
                    if !generics.is_empty() {
                        return ctx.add_invalid_generics(
                            self.span,
                            GenericDataTypeKind::Float,
                            0,
                            generics.len(),
                        );
                    }
                }
                "double" => {
                    if !generics.is_empty() {
                        return ctx.add_invalid_generics(
                            self.span,
                            GenericDataTypeKind::Double,
                            0,
                            generics.len(),
                        );
                    }
                }
                "string" | "str" => {
                    if !generics.is_empty() {
                        return ctx.add_invalid_generics(
                            self.span,
                            GenericDataTypeKind::String,
                            0,
                            generics.len(),
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
                    if !generics.is_empty() {
                        return ctx.add_invalid_generics(
                            self.span,
                            GenericDataTypeKind::Score,
                            0,
                            generics.len(),
                        );
                    }
                }
                "list" => {
                    if generics.len() != 1 {
                        return ctx.add_invalid_generics(
                            self.span,
                            GenericDataTypeKind::List,
                            1,
                            generics.len(),
                        );
                    } else if let Some(first) = generics.first() {
                        return first.perform_semantic_analysis(ctx);
                    }
                }
                "compound" => {
                    if generics.len() != 1 {
                        return ctx.add_invalid_generics(
                            self.span,
                            GenericDataTypeKind::Compound,
                            1,
                            generics.len(),
                        );
                    } else if let Some(first) = generics.first() {
                        return first.perform_semantic_analysis(ctx);
                    }
                }
                "data" => {
                    if generics.len() != 1 {
                        return ctx.add_invalid_generics(
                            self.span,
                            GenericDataTypeKind::Data,
                            1,
                            generics.len(),
                        );
                    } else if let Some(first) = generics.first() {
                        return first.perform_semantic_analysis(ctx);
                    }
                }
                "any" => {
                    if !generics.is_empty() {
                        return ctx.add_invalid_generics(
                            self.span,
                            GenericDataTypeKind::Any,
                            0,
                            generics.len(),
                        );
                    }
                }
                _ => {
                    if ctx.get_variable(name).is_none() {
                        return ctx.add_info(SemanticAnalysisInfo {
                            span: *span,
                            kind: SemanticAnalysisInfoKind::Error(
                                SemanticAnalysisError::UnknownType(name.clone()),
                            ),
                        });
                    }
                }
            },
            HighDataTypeKind::Unit => (),
            HighDataTypeKind::Reference(data_type) => {
                return data_type.perform_semantic_analysis(ctx);
            }
            HighDataTypeKind::TypedCompound(compound) => {
                return compound
                    .values()
                    .map(|value| value.perform_semantic_analysis(ctx))
                    .all_some();
            }
        }

        Some(())
    }
}
