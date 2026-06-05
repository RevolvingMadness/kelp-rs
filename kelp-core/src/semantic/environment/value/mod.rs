use crate::make_id;
use crate::parsed::semantic_analysis::info::error::{ItemKind, SemanticAnalysisError};
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::SemanticEnvironment;
use crate::semantic::environment::r#type::module::HighModuleId;
use crate::semantic::environment::value::{
    function::{SemanticFunctionDeclaration, regular::HighRegularFunctionId},
    variable::SemanticVariableDeclaration,
};
use crate::{
    parsed::semantic_analysis::SemanticAnalysisContext, span::Span, visibility::Visibility,
};

pub mod function;
pub mod variable;

make_id!(HighValueId);

impl HighValueId {
    pub fn assert_visible_result(
        self,
        semantic_environment: &SemanticEnvironment,
        current_module_path: &[HighModuleId],
    ) -> Result<HighVisibleValueId, SemanticAnalysisError> {
        let declaration = semantic_environment.get_value(self);

        if !declaration.is_visible(current_module_path) {
            return Err(SemanticAnalysisError::ValueNotPublic(
                declaration.kind.name().to_owned(),
            ));
        }

        Ok(HighVisibleValueId(self.0))
    }
}

make_id!(HighVisibleValueId);

impl From<HighVisibleValueId> for HighValueId {
    fn from(value: HighVisibleValueId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub enum SemanticValueDeclarationKind {
    Variable(SemanticVariableDeclaration),
    Function(Box<SemanticFunctionDeclaration>),
}

impl SemanticValueDeclarationKind {
    #[must_use]
    pub fn name_span(&self) -> Option<Span> {
        match self {
            Self::Variable(declaration) => Some(declaration.name_span),
            Self::Function(declaration) => declaration.name_span(),
        }
    }

    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Variable(declaration) => &declaration.name,
            Self::Function(declaration) => declaration.name(),
        }
    }

    #[must_use]
    pub fn generic_count(&self) -> usize {
        match self {
            Self::Variable(..) => 0,
            Self::Function(declaration) => declaration.declared_generic_count(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SemanticValueDeclaration {
    pub visibility: Visibility,
    pub module_path: Vec<HighModuleId>,
    pub kind: SemanticValueDeclarationKind,
}

impl SemanticValueDeclaration {
    #[must_use]
    pub fn is_visible(&self, current_module_path: &[HighModuleId]) -> bool {
        if matches!(self.visibility, Visibility::Public) {
            return true;
        }

        current_module_path.starts_with(&self.module_path)
    }

    pub fn into_data_type(
        self,
        ctx: &mut SemanticAnalysisContext,
        original_id: HighVisibleValueId,
        inherited_generic_types: Vec<SemanticDataType>,
        supplied_generic_types: &[SemanticDataType],
        path_span: Span,
    ) -> Option<SemanticDataType> {
        match self.kind {
            SemanticValueDeclarationKind::Variable(declaration) => {
                let expected_generics = 0;
                let actual_generics = supplied_generic_types.len();

                if actual_generics != expected_generics {
                    let type_name = &declaration
                        .data_type
                        .display(&ctx.semantic_environment)
                        .to_string();

                    return ctx.add_invalid_generics(
                        path_span,
                        Some(declaration.name_span),
                        expected_generics,
                        actual_generics,
                    );
                }

                Some(declaration.data_type)
            }
            SemanticValueDeclarationKind::Function(declaration) => {
                let id = HighRegularFunctionId(original_id.0);

                let expected_generics = declaration.declared_generic_count();
                let actual_generics = supplied_generic_types.len();

                if actual_generics != expected_generics {
                    return ctx.add_invalid_generics(
                        path_span,
                        declaration.name_span(),
                        expected_generics,
                        actual_generics,
                    );
                }

                let mut generic_types = inherited_generic_types;

                generic_types.extend(supplied_generic_types.iter().cloned());

                Some(SemanticDataType::Function(id.into(), generic_types))
            }
        }
    }
}
