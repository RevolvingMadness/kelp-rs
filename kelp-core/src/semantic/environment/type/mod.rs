use crate::make_id;
use crate::parsed::semantic_analysis::SemanticAnalysisContext;
use crate::parsed::semantic_analysis::info::error::{SemanticAnalysisError, TypeKind};
use crate::path::generic::TypedPathSegment;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::SemanticEnvironment;
use crate::semantic::environment::r#type::generic::SemanticGenericDeclaration;
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

make_id!(HighGenericId);

impl From<HighGenericId> for HighTypeId {
    fn from(value: HighGenericId) -> Self {
        Self(value.0)
    }
}

make_id!(HighTypeId);

impl HighTypeId {
    pub fn assert_visible_result(
        self,
        semantic_environment: &SemanticEnvironment,
        current_module_path: &[HighModuleId],
    ) -> Result<HighVisibleTypeId, SemanticAnalysisError> {
        let declaration = semantic_environment.get_type(self);

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
    pub const fn get_type_kind(&self) -> TypeKind {
        match self {
            Self::Module(..) => TypeKind::Module,
            Self::Struct(..) => TypeKind::Struct,
            Self::Alias(..) => TypeKind::Alias,
            Self::Generic(..) => TypeKind::Generic,
            Self::Builtin(..) => TypeKind::Builtin,
        }
    }

    #[must_use]
    pub const fn name_span(&self) -> Option<Span> {
        Some(match self {
            Self::Module(declaration) => return declaration.name_span,
            Self::Struct(declaration) => declaration.name_span(),
            Self::Alias(declaration) => declaration.name_span,
            Self::Generic(declaration) => declaration.name_span,
            Self::Builtin(..) => return None,
        })
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

    #[must_use]
    pub fn generic_ids(&self) -> &[HighGenericId] {
        match self {
            Self::Module(..) => &[],
            Self::Struct(declaration) => declaration.generic_ids(),
            Self::Alias(declaration) => &declaration.generic_ids,
            Self::Generic(_) => &[],
            Self::Builtin(..) => &[],
        }
    }

    #[must_use]
    pub const fn generic_count(&self) -> usize {
        match self {
            Self::Module(..) => 0,
            Self::Struct(declaration) => declaration.generic_count(),
            Self::Alias(declaration) => declaration.generic_count(),
            Self::Generic(_) => 0,
            Self::Builtin(declaration) => declaration.generic_count,
        }
    }

    pub fn into_data_type(
        self,
        ctx: &mut SemanticAnalysisContext,
        id: HighVisibleTypeId,
        segment: &TypedPathSegment<SemanticDataType>,
    ) -> SemanticDataType {
        match self {
            Self::Module(SemanticModuleDeclaration { name, .. }) => {
                ctx.add_error_type(SemanticAnalysisError::NotAType {
                    type_span: segment.name_span,
                    type_name: name,
                })
            }
            Self::Struct(declaration) => {
                let id = HighStructId(id.0);

                let expected_generic_count = declaration.generic_count();
                let actual_generic_count = segment.generic_types.len();

                if actual_generic_count != expected_generic_count {
                    return ctx.add_error_type(SemanticAnalysisError::InvalidGenerics {
                        type_name_span: segment.name_span,
                        type_kind: TypeKind::Struct.into(),
                        declaration_span: Some(declaration.name_span()),
                        expected: expected_generic_count,
                        actual: actual_generic_count,
                    });
                }

                SemanticDataType::Struct(id, segment.generic_types.clone())
            }
            Self::Alias(declaration) => {
                let expected_generic_count = declaration.generic_ids.len();
                let actual_generic_count = segment.generic_types.len();

                if actual_generic_count != expected_generic_count {
                    return ctx.add_error_type(SemanticAnalysisError::InvalidGenerics {
                        type_name_span: segment.name_span,
                        type_kind: TypeKind::Alias.into(),
                        declaration_span: Some(declaration.name_span),
                        expected: expected_generic_count,
                        actual: actual_generic_count,
                    });
                }

                declaration
                    .alias
                    .substitute_generics(&declaration.generic_ids, &segment.generic_types)
            }
            Self::Generic(declaration) => {
                let expected_generic_count = 0;
                let actual_generic_count = segment.generic_types.len();

                if actual_generic_count != expected_generic_count {
                    return ctx.add_error_type(SemanticAnalysisError::InvalidGenerics {
                        type_name_span: segment.name_span,
                        type_kind: TypeKind::Generic.into(),
                        declaration_span: Some(declaration.name_span),
                        expected: expected_generic_count,
                        actual: actual_generic_count,
                    });
                }

                SemanticDataType::Generic(HighGenericId(id.0))
            }
            Self::Builtin(data_type) => data_type.into_data_type(ctx, segment),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SemanticTypeDeclaration {
    pub module_path: Vec<HighModuleId>,
    pub visibility: Visibility,
    pub kind: SemanticTypeDeclarationKind,
}

impl SemanticTypeDeclaration {
    #[must_use]
    pub fn is_visible(&self, current_module_path: &[HighModuleId]) -> bool {
        if matches!(self.visibility, Visibility::Public) {
            return true;
        }

        current_module_path.starts_with(&self.module_path)
    }

    pub fn get_visible_type_id(
        &self,
        semantic_environment: &SemanticEnvironment,
        current_module_path: &[HighModuleId],
        self_id: HighVisibleTypeId,
        self_name_span: Span,
        name: &str,
        name_span: Span,
    ) -> Result<HighVisibleTypeId, SemanticAnalysisError> {
        match &self.kind {
            SemanticTypeDeclarationKind::Module(declaration) => declaration.get_visible_type_id(
                semantic_environment,
                current_module_path,
                name,
                name_span,
            ),
            SemanticTypeDeclarationKind::Struct(declaration) => declaration.get_visible_type_id(
                semantic_environment,
                current_module_path,
                self_id,
                name,
                name_span,
            ),
            _ => Err(SemanticAnalysisError::TypeDoesntContainItems {
                type_span: self_name_span,
                type_kind: self.kind.get_type_kind(),
            }),
        }
    }

    pub fn get_visible_value_id(
        &self,
        semantic_environment: &SemanticEnvironment,
        current_module_path: &[HighModuleId],
        self_id: HighVisibleTypeId,
        base_span: Span,
        name: &str,
        name_span: Span,
    ) -> Result<HighVisibleValueId, SemanticAnalysisError> {
        match &self.kind {
            SemanticTypeDeclarationKind::Module(declaration) => declaration.get_visible_value_id(
                semantic_environment,
                current_module_path,
                name,
                name_span,
            ),
            SemanticTypeDeclarationKind::Struct(declaration) => declaration.get_visible_value_id(
                semantic_environment,
                current_module_path,
                self_id,
                name,
                name_span,
            ),
            _ => Err(SemanticAnalysisError::TypeDoesntContainItems {
                type_span: base_span,
                type_kind: self.kind.get_type_kind(),
            }),
        }
    }
}
