use crate::{
    high::{
        environment::value::{
            function::{
                HighFunctionDeclaration, HighFunctionId, builtin::HighBuiltinFunctionId,
                regular::HighRegularFunctionId,
            },
            variable::{HighVariableDeclaration, HighVariableId},
        },
        semantic_analysis::SemanticAnalysisContext,
    },
    low::data_type::unresolved::UnresolvedDataType,
    span::Span,
    visibility::Visibility,
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
pub enum HighValueDeclarationKind {
    Variable(HighVariableDeclaration),
    Function(HighFunctionDeclaration),
}

impl HighValueDeclarationKind {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Variable(declaration) => &declaration.name,
            Self::Function(declaration) => declaration.name(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct HighValueDeclaration {
    pub visibility: Visibility,
    pub module_path: Vec<String>,
    pub kind: HighValueDeclarationKind,
}

impl HighValueDeclaration {
    pub fn resolve_fully(
        self,
        ctx: &mut SemanticAnalysisContext,
        original_id: HighValueId,
        generic_types: Vec<UnresolvedDataType>,
        path_span: Span,
    ) -> Option<(HighValueId, UnresolvedDataType)> {
        match self.kind {
            HighValueDeclarationKind::Variable(declaration) => {
                let expected_generics = 0;
                let actual_generics = generic_types.len();

                if actual_generics != expected_generics {
                    let type_name = &declaration
                        .data_type
                        .display(&ctx.high_environment)
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
            HighValueDeclarationKind::Function(declaration) => {
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
                    UnresolvedDataType::Function(id.into(), generic_types),
                ))
            }
        }
    }

    #[must_use]
    pub fn data_type(
        &self,
        id: HighValueId,
        generic_types: &[UnresolvedDataType],
    ) -> UnresolvedDataType {
        match &self.kind {
            HighValueDeclarationKind::Variable(declaration) => declaration.data_type.clone(),
            HighValueDeclarationKind::Function(..) => {
                UnresolvedDataType::Function(HighFunctionId(id.0), generic_types.to_vec())
            }
        }
    }
}
