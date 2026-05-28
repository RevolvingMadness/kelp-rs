use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::value::{
    function::{HighFunctionId, SemanticFunctionDeclaration, regular::HighRegularFunctionId},
    variable::SemanticVariableDeclaration,
};
use crate::{
    parsed::semantic_analysis::SemanticAnalysisContext, span::Span, visibility::Visibility,
};

pub mod function;
pub mod variable;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighValueId(pub u32);

#[derive(Debug, Clone)]
pub enum SemanticValueDeclarationKind {
    Variable(SemanticVariableDeclaration),
    Function(Box<SemanticFunctionDeclaration>),
}

impl SemanticValueDeclarationKind {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Variable(declaration) => &declaration.name,
            Self::Function(declaration) => declaration.name(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SemanticValueDeclaration {
    pub visibility: Visibility,
    pub module_path: Vec<String>,
    pub kind: SemanticValueDeclarationKind,
}

impl SemanticValueDeclaration {
    #[must_use]
    pub fn is_visible(&self, current_module_path: &[String]) -> bool {
        if matches!(self.visibility, Visibility::Public) {
            return true;
        }

        current_module_path.starts_with(&self.module_path)
    }

    pub fn resolve_fully(
        self,
        ctx: &mut SemanticAnalysisContext,
        original_id: HighValueId,
        generic_types: Vec<SemanticDataType>,
        path_span: Span,
    ) -> Option<(HighValueId, SemanticDataType)> {
        match self.kind {
            SemanticValueDeclarationKind::Variable(declaration) => {
                let expected_generics = 0;
                let actual_generics = generic_types.len();

                if actual_generics != expected_generics {
                    let type_name = &declaration
                        .data_type
                        .display(&ctx.semantic_environment)
                        .to_string();

                    return ctx.add_invalid_generics(
                        path_span,
                        type_name,
                        expected_generics,
                        actual_generics,
                    );
                }

                Some((original_id, declaration.data_type))
            }
            SemanticValueDeclarationKind::Function(declaration) => {
                let id = HighRegularFunctionId(original_id.0);

                let expected_generics = declaration.generic_count();
                let actual_generics = generic_types.len();

                if actual_generics != expected_generics {
                    return ctx.add_invalid_generics(
                        path_span,
                        declaration.name(),
                        expected_generics,
                        actual_generics,
                    );
                }

                Some((
                    original_id,
                    SemanticDataType::Function(id.into(), generic_types),
                ))
            }
        }
    }

    #[must_use]
    pub fn data_type(
        &self,
        id: HighValueId,
        generic_types: &[SemanticDataType],
    ) -> SemanticDataType {
        match &self.kind {
            SemanticValueDeclarationKind::Variable(declaration) => declaration.data_type.clone(),
            SemanticValueDeclarationKind::Function(..) => {
                SemanticDataType::Function(HighFunctionId(id.0), generic_types.to_vec())
            }
        }
    }
}
