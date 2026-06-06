use crate::make_id;
use crate::parsed::environment::ParsedEnvironment;
use crate::parsed::semantic_analysis::SemanticAnalysisContext;
use crate::parsed::semantic_analysis::info::error::{SemanticAnalysisError, TypeKind};
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::SemanticEnvironment;
use crate::semantic::environment::r#type::generic::{HighGenericId, SemanticGenericDeclaration};
use crate::semantic::environment::r#type::module::HighModuleId;
use crate::semantic::environment::r#type::r#struct::HighStructId;
use crate::semantic::environment::r#type::{
    alias::SemanticTypeAliasDeclaration, builtin_data_type::SemanticBuiltinTypeDeclaration,
    module::SemanticModuleDeclaration, r#struct::SemanticStructDeclaration,
};
use crate::semantic::environment::value::HighVisibleValueId;
use crate::span::Span;
use crate::visibility::Visibility;

pub mod alias;
pub mod builtin_data_type;
pub mod generic;
pub mod module;
pub mod r#struct;

make_id!(HighTypeId);

impl HighTypeId {
    pub fn assert_visible_result(
        self,
        parsed_environment: &ParsedEnvironment,
        current_module_path: &[HighModuleId],
    ) -> Result<HighVisibleTypeId, SemanticAnalysisError> {
        let declaration = parsed_environment.get_type(self);

        if !declaration.is_visible(current_module_path) {
            return Err(SemanticAnalysisError::TypeNotPublic(
                declaration.kind.name().to_owned(),
            ));
        }

        Ok(HighVisibleTypeId(self.0))
    }
}

make_id!(HighVisibleTypeId);

impl From<HighVisibleTypeId> for HighTypeId {
    fn from(value: HighVisibleTypeId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub enum SemanticTypeDeclarationKind {
    Module(SemanticModuleDeclaration),
    Struct(SemanticStructDeclaration),
    Alias(SemanticTypeAliasDeclaration),
    Generic(SemanticGenericDeclaration),
    Builtin(SemanticBuiltinTypeDeclaration),
}

impl SemanticTypeDeclarationKind {
    #[must_use]
    pub const fn get_kind(&self) -> TypeKind {
        match self {
            Self::Module(..) => TypeKind::Module,
            Self::Struct(..) => TypeKind::Struct,
            Self::Alias(..) => TypeKind::Alias,
            Self::Generic(..) => TypeKind::Generic,
            Self::Builtin(..) => TypeKind::Builtin,
        }
    }

    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Module(declaration) => &declaration.name,
            Self::Struct(declaration) => declaration.name(),
            Self::Alias(declaration) => &declaration.name,
            Self::Generic(declaration) => &declaration.name,
            Self::Builtin(data_type) => &data_type.name,
        }
    }

    pub fn get_visible_type_id(
        &self,
        parsed_environment: &ParsedEnvironment,
        semantic_environment: &SemanticEnvironment,
        current_module_path: &[HighModuleId],
        self_id: HighVisibleTypeId,
        self_name_span: Span,
        name: &str,
        name_span: Span,
    ) -> Result<HighVisibleTypeId, SemanticAnalysisError> {
        match self {
            SemanticTypeDeclarationKind::Module(declaration) => declaration.get_visible_type_id(
                parsed_environment,
                current_module_path,
                name,
                name_span,
            ),
            SemanticTypeDeclarationKind::Struct(declaration) => declaration.get_visible_type_id(
                parsed_environment,
                semantic_environment,
                current_module_path,
                self_id,
                name,
                name_span,
            ),
            _ => Err(SemanticAnalysisError::TypeDoesntContainItems {
                type_span: self_name_span,
                type_kind: self.get_kind(),
            }),
        }
    }

    pub fn get_visible_value_id(
        &self,
        parsed_environment: &ParsedEnvironment,
        semantic_environment: &SemanticEnvironment,
        current_module_path: &[HighModuleId],
        self_id: HighVisibleTypeId,
        base_span: Span,
        name: &str,
        name_span: Span,
    ) -> Result<HighVisibleValueId, SemanticAnalysisError> {
        match self {
            SemanticTypeDeclarationKind::Module(declaration) => declaration.get_visible_value_id(
                parsed_environment,
                current_module_path,
                name,
                name_span,
            ),
            SemanticTypeDeclarationKind::Struct(declaration) => declaration.get_visible_value_id(
                parsed_environment,
                semantic_environment,
                current_module_path,
                self_id,
                name,
                name_span,
            ),
            _ => Err(SemanticAnalysisError::TypeDoesntContainItems {
                type_span: base_span,
                type_kind: self.get_kind(),
            }),
        }
    }

    pub fn into_data_type(
        self,
        ctx: &mut SemanticAnalysisContext,
        self_id: HighVisibleTypeId,
        generic_spans: &[Span],
        generic_types: &[SemanticDataType],
        name_span: Span,
    ) -> SemanticDataType {
        match self {
            Self::Module(..) => ctx.add_error_type(SemanticAnalysisError::NotAType {
                span: name_span,
                kind: TypeKind::Module,
            }),
            Self::Struct(declaration) => {
                let id = HighStructId(self_id.0);

                let expected_generic_count = declaration.generic_count();
                let actual_generic_count = generic_types.len();

                if actual_generic_count != expected_generic_count {
                    return ctx.add_error_type(SemanticAnalysisError::InvalidGenerics {
                        type_name_span: name_span,
                        item_kind: TypeKind::Struct.into(),
                        declaration_span: Some(declaration.name_span()),
                        expected: expected_generic_count,
                        actual: actual_generic_count,
                    });
                }

                SemanticDataType::Struct(id, generic_types.to_vec())
            }
            Self::Alias(declaration) => {
                let expected_generic_count = declaration.generic_ids.len();
                let actual_generic_count = generic_types.len();

                if actual_generic_count != expected_generic_count {
                    return ctx.add_error_type(SemanticAnalysisError::InvalidGenerics {
                        type_name_span: name_span,
                        item_kind: TypeKind::Alias.into(),
                        declaration_span: Some(declaration.name_span),
                        expected: expected_generic_count,
                        actual: actual_generic_count,
                    });
                }

                declaration
                    .alias
                    .substitute_generics(&declaration.generic_ids, generic_types)
            }
            Self::Generic(declaration) => {
                let expected_generic_count = 0;
                let actual_generic_count = generic_types.len();

                if actual_generic_count != expected_generic_count {
                    return ctx.add_error_type(SemanticAnalysisError::InvalidGenerics {
                        type_name_span: name_span,
                        item_kind: TypeKind::Generic.into(),
                        declaration_span: Some(declaration.name_span),
                        expected: expected_generic_count,
                        actual: actual_generic_count,
                    });
                }

                SemanticDataType::Generic(HighGenericId(self_id.0))
            }
            Self::Builtin(data_type) => {
                data_type.into_data_type(ctx, generic_spans, generic_types, name_span)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct SemanticTypeDeclaration {
    pub module_path: Vec<HighModuleId>,
    pub visibility: Visibility,
    pub kind: SemanticTypeDeclarationKind,
}
