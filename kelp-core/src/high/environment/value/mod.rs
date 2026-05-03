use crate::{
    high::{
        data_type::resolved::GenericResolver,
        environment::value::{
            function::{HighFunctionDeclaration, HighFunctionId},
            variable::{HighVariableDeclaration, HighVariableId},
        },
        semantic_analysis::SemanticAnalysisContext,
    },
    low::{
        data_type::DataType,
        environment::value::{ValueId, function::FunctionDeclaration},
    },
    span::Span,
    visibility::Visibility,
};

pub mod function;
pub mod variable;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighValueId(pub usize);

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
            Self::Function(declaration) => &declaration.name,
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
        generic_types: Vec<DataType>,
        path_span: Span,
    ) -> Option<(ValueId, DataType)> {
        match self.kind {
            HighValueDeclarationKind::Variable(declaration) => {
                let data_type = declaration.data_type?;

                if let Some(resolved_id) = ctx.get_resolved_variable(original_id) {
                    return Some((resolved_id.into(), data_type));
                }

                let resolved_id = ctx.environment.declare_variable(
                    self.module_path,
                    self.visibility,
                    declaration.name,
                    data_type.clone(),
                );

                ctx.declare_resolved_variable(original_id, resolved_id);

                Some((resolved_id.into(), data_type))
            }
            HighValueDeclarationKind::Function(declaration) => {
                let original_id = HighFunctionId(original_id.0);

                if let Some(id) = ctx.get_monomorphized_function_id(original_id, &generic_types) {
                    return Some((id.into(), DataType::Function(id)));
                }

                let resolver = GenericResolver::create_semantic_analysis(
                    ctx,
                    &declaration.name,
                    path_span,
                    &declaration.generic_names,
                    &generic_types,
                )?;

                let parameters = declaration
                    .parameters
                    .into_iter()
                    .map(|(pattern, data_type)| {
                        (
                            pattern,
                            data_type.map(|data_type| data_type.resolve_fully(&resolver)),
                        )
                    })
                    .collect();

                let return_type = declaration.return_type?.resolve_fully(&resolver);

                let monomorphized_id = ctx.declare_monomorphized_function(
                    original_id,
                    self.visibility,
                    FunctionDeclaration {
                        module_path: self.module_path,
                        name: declaration.name,
                        generic_types,
                        parameters,
                        return_type,
                        body: declaration.body,
                    },
                );

                Some((
                    monomorphized_id.into(),
                    DataType::Function(monomorphized_id),
                ))
            }
        }
    }

    #[must_use]
    pub const fn data_type(&self) -> Option<Option<&DataType>> {
        match &self.kind {
            HighValueDeclarationKind::Variable(declaration) => Some(declaration.data_type.as_ref()),
            HighValueDeclarationKind::Function(_) => None,
        }
    }
}
