use crate::{
    parsed::semantic_analysis::SemanticAnalysisContext,
    span::Span,
    visibility::Visibility,
};
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::value::{
    function::{
        builtin::HighBuiltinFunctionId, regular::HighRegularFunctionId, HighFunctionId,
        SemanticFunctionDeclaration,
    },
    variable::{HighVariableId, SemanticVariableDeclaration},
};

pub mod function;
pub mod variable;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighValueId(pub u32);

impl From<HighVariableId> for HighValueId {
    fn from(value: HighVariableId) -> Self {
        Self(value.0)
    }
}

impl From<HighFunctionId> for HighValueId {
    fn from(value: HighFunctionId) -> Self {
        Self(value.0)
    }
}

impl From<HighRegularFunctionId> for HighValueId {
    fn from(value: HighRegularFunctionId) -> Self {
        Self(value.0)
    }
}

impl From<HighBuiltinFunctionId> for HighValueId {
    fn from(value: HighBuiltinFunctionId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub enum ResolvedValueDeclarationKind {
    Variable(SemanticVariableDeclaration),
    Function(Box<SemanticFunctionDeclaration>),
}

impl ResolvedValueDeclarationKind {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Variable(declaration) => &declaration.name,
            Self::Function(declaration) => declaration.name(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedValueDeclaration {
    pub visibility: Visibility,
    pub module_path: Vec<String>,
    pub kind: ResolvedValueDeclarationKind,
}

impl ResolvedValueDeclaration {
    pub fn resolve_fully(
        self,
        ctx: &mut SemanticAnalysisContext,
        original_id: HighValueId,
        generic_types: Vec<SemanticDataType>,
        path_span: Span,
    ) -> Option<(HighValueId, SemanticDataType)> {
        match self.kind {
            ResolvedValueDeclarationKind::Variable(declaration) => {
                let expected_generics = 0;
                let actual_generics = generic_types.len();

                if actual_generics != expected_generics {
                    let type_name = &declaration
                        .data_type
                        .display(&ctx.resolved_environment)
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
            ResolvedValueDeclarationKind::Function(declaration) => {
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
            ResolvedValueDeclarationKind::Variable(declaration) => declaration.data_type.clone(),
            ResolvedValueDeclarationKind::Function(..) => {
                SemanticDataType::Function(HighFunctionId(id.0), generic_types.to_vec())
            }
        }
    }
}
