use strum::{Display, EnumIter};

use crate::{
    make_id,
    parsed::semantic_analysis::{
        SemanticAnalysisContext,
        info::error::{SemanticAnalysisError, TypeKind},
    },
    path::generic::TypedPathSegment,
    semantic::data_type::SemanticDataType,
};

make_id!(HighBuiltinTypeId);

#[derive(Debug, Clone)]
pub struct SemanticBuiltinTypeDeclaration {
    pub name: String,
    pub generic_count: usize,
    pub kind: BuiltinTypeKind,
}

impl SemanticBuiltinTypeDeclaration {
    #[must_use]
    pub fn into_data_type(
        self,
        ctx: &mut SemanticAnalysisContext,
        segment: &TypedPathSegment<SemanticDataType>,
    ) -> SemanticDataType {
        let expected_generic_count = self.generic_count;
        let actual_generic_count = segment.generic_types.len();

        if actual_generic_count != expected_generic_count {
            return ctx.add_error_type(SemanticAnalysisError::InvalidGenerics {
                type_name_span: segment.name_span,
                type_kind: TypeKind::Builtin.into(),
                declaration_span: None,
                expected: expected_generic_count,
                actual: actual_generic_count,
            });
        }

        match self.kind {
            BuiltinTypeKind::Boolean => SemanticDataType::Boolean,
            BuiltinTypeKind::Byte => SemanticDataType::Byte,
            BuiltinTypeKind::Short => SemanticDataType::Short,
            BuiltinTypeKind::Integer => SemanticDataType::Integer,
            BuiltinTypeKind::Long => SemanticDataType::Long,
            BuiltinTypeKind::Float => SemanticDataType::Float,
            BuiltinTypeKind::Double => SemanticDataType::Double,
            BuiltinTypeKind::String => SemanticDataType::String,
            BuiltinTypeKind::List => {
                let element_type = segment.generic_types[0].clone();

                SemanticDataType::List(Box::new(element_type))
            }
            BuiltinTypeKind::Compound => {
                let element_type = segment.generic_types[0].clone();

                SemanticDataType::Compound(Box::new(element_type))
            }
            BuiltinTypeKind::Data => {
                let element_type = segment.generic_types[0].clone();

                let Some(data_type) = element_type.get_data_type(&ctx.semantic_environment) else {
                    let element_span = segment.generic_spans[0];

                    return ctx.add_error_type(SemanticAnalysisError::TypeIsNotDataCompatible {
                        type_span: element_span,
                        data_type: element_type,
                    });
                };

                SemanticDataType::Data(Box::new(data_type))
            }
            BuiltinTypeKind::Score => {
                let element_type = segment.generic_types[0].clone();

                if !element_type.is_score_compatible() {
                    let element_span = segment.generic_spans[0];

                    return ctx.add_error_type(SemanticAnalysisError::TypeIsNotScoreCompatible {
                        type_span: element_span,
                        data_type: element_type,
                    });
                }

                SemanticDataType::Score(Box::new(element_type))
            }
            BuiltinTypeKind::EntitySelector => SemanticDataType::EntitySelector,
            BuiltinTypeKind::ResourceLocation => SemanticDataType::ResourceLocation,
            BuiltinTypeKind::Coordinates => SemanticDataType::Coordinates,
        }
    }
}

#[derive(Display, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumIter)]
pub enum BuiltinTypeKind {
    Boolean,
    Byte,
    Short,
    Integer,
    Long,
    Float,
    Double,
    String,
    Score,
    List,
    Compound,
    Data,
    EntitySelector,
    ResourceLocation,
    Coordinates,
}

impl BuiltinTypeKind {
    #[must_use]
    pub fn declaration(self) -> SemanticBuiltinTypeDeclaration {
        macro_rules! declaration {
            ($name:ident) => {
                declaration!($name<0>)
            };

            ($name:ident<$generic_count:literal>) => {
                SemanticBuiltinTypeDeclaration {
                    name: stringify!($name).to_owned(),
                    generic_count: $generic_count,
                    kind: self,
                }
            };
        }

        match self {
            Self::Boolean => declaration!(bool),
            Self::Byte => declaration!(byte),
            Self::Short => declaration!(short),
            Self::Integer => declaration!(integer),
            Self::Long => declaration!(long),
            Self::Float => declaration!(float),
            Self::Double => declaration!(double),
            Self::String => declaration!(string),
            Self::Score => declaration!(score<1>),
            Self::List => declaration!(list<1>),
            Self::Compound => declaration!(compound<1>),
            Self::Data => declaration!(data<1>),
            Self::EntitySelector => declaration!(entity_selector),
            Self::ResourceLocation => declaration!(resource_location),
            Self::Coordinates => declaration!(coordinates),
        }
    }
}
