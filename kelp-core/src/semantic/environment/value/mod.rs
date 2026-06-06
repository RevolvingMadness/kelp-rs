use crate::make_id;
use crate::parsed::semantic_analysis::info::error::{SemanticAnalysisError, ValueKind};
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::SemanticEnvironment;
use crate::semantic::environment::r#type::module::HighModuleId;
use crate::semantic::environment::value::constant::SemanticConstantDeclaration;
use crate::semantic::environment::value::{
    function::{SemanticFunctionDeclaration, regular::HighRegularFunctionId},
    variable::SemanticVariableDeclaration,
};
use crate::{
    parsed::semantic_analysis::SemanticAnalysisContext, span::Span, visibility::Visibility,
};

pub mod constant;
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
    Constant(SemanticConstantDeclaration),
    Function(SemanticFunctionDeclaration),
}

impl SemanticValueDeclarationKind {
    #[must_use]
    pub const fn get_value_kind(&self) -> ValueKind {
        match self {
            Self::Variable(..) => ValueKind::Variable,
            Self::Constant(..) => ValueKind::Constant,
            Self::Function(..) => ValueKind::Function,
        }
    }

    #[must_use]
    pub fn name_span(&self) -> Option<Span> {
        match self {
            Self::Variable(declaration) => Some(declaration.name_span),
            Self::Constant(declaration) => Some(declaration.name_span),
            Self::Function(declaration) => declaration.name_span(),
        }
    }

    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Variable(declaration) => &declaration.name,
            Self::Constant(declaration) => &declaration.name,
            Self::Function(declaration) => declaration.name(),
        }
    }

    #[must_use]
    pub fn generic_count(&self) -> usize {
        match self {
            Self::Variable(..) => 0,
            Self::Constant(..) => 0,
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
        name_span: Span,
    ) -> Option<SemanticDataType> {
        match self.kind {
            SemanticValueDeclarationKind::Variable(declaration) => {
                let expected_generic_count = 0;
                let actual_generic_count = supplied_generic_types.len();

                if actual_generic_count != expected_generic_count {
                    return ctx.add_error(SemanticAnalysisError::InvalidGenerics {
                        type_name_span: name_span,
                        item_kind: ValueKind::Variable.into(),
                        declaration_span: Some(declaration.name_span),
                        expected: expected_generic_count,
                        actual: actual_generic_count,
                    });
                }

                Some(declaration.data_type)
            }
            SemanticValueDeclarationKind::Constant(declaration) => {
                let expected_generic_count = 0;
                let actual_generic_count = supplied_generic_types.len();

                if actual_generic_count != expected_generic_count {
                    return ctx.add_error(SemanticAnalysisError::InvalidGenerics {
                        type_name_span: name_span,
                        item_kind: ValueKind::Variable.into(),
                        declaration_span: Some(declaration.name_span),
                        expected: expected_generic_count,
                        actual: actual_generic_count,
                    });
                }

                Some(declaration.data_type)
            }
            SemanticValueDeclarationKind::Function(declaration) => {
                let id = HighRegularFunctionId(original_id.0);

                let expected_generic_count = declaration.declared_generic_count();
                let actual_generic_count = supplied_generic_types.len();

                if actual_generic_count != expected_generic_count {
                    return ctx.add_error(SemanticAnalysisError::InvalidGenerics {
                        type_name_span: name_span,
                        item_kind: ValueKind::Function.into(),
                        declaration_span: declaration.name_span(),
                        expected: expected_generic_count,
                        actual: actual_generic_count,
                    });
                }

                let mut generic_types = inherited_generic_types;

                generic_types.extend(supplied_generic_types.iter().cloned());

                Some(SemanticDataType::Function(id.into(), generic_types))
            }
        }
    }
}
