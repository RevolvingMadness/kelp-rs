use crate::make_id;
use crate::parsed::environment::ParsedEnvironment;
use crate::parsed::semantic_analysis::info::error::{SemanticAnalysisError, ValueKind};
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::module::HighModuleId;
use crate::semantic::environment::value::constant::SemanticConstantDeclaration;
use crate::semantic::environment::value::{
    function::{SemanticFunctionDeclaration, regular::HighRegularFunctionId},
    variable::SemanticVariableDeclaration,
};
use crate::{parsed::semantic_analysis::SemanticAnalysisContext, span::Span};

pub mod constant;
pub mod function;
pub mod variable;

make_id!(HighValueId);

impl HighValueId {
    pub fn assert_visible_result(
        self,
        parsed_environment: &ParsedEnvironment,
        current_module_path: &[HighModuleId],
    ) -> Result<HighVisibleValueId, SemanticAnalysisError> {
        let declaration = parsed_environment.get_value(self);

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
pub enum SemanticValueDeclaration {
    Variable(SemanticVariableDeclaration),
    Constant(SemanticConstantDeclaration),
    Function(SemanticFunctionDeclaration),
}

impl SemanticValueDeclaration {
    #[must_use]
    pub const fn get_value_kind(&self) -> ValueKind {
        match self {
            Self::Variable(..) => ValueKind::Variable,
            Self::Constant(..) => ValueKind::Constant,
            Self::Function(..) => ValueKind::Function,
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

    pub fn into_data_type(
        self,
        ctx: &mut SemanticAnalysisContext,
        original_id: HighVisibleValueId,
        inherited_generic_types: Vec<SemanticDataType>,
        supplied_generic_types: &[SemanticDataType],
        name_span: Span,
    ) -> Option<SemanticDataType> {
        match self {
            Self::Variable(declaration) => {
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
            Self::Constant(declaration) => {
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
            SemanticValueDeclaration::Function(declaration) => {
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
