use crate::{
    parsed::{
        environment::r#type::{
            alias::ParsedTypeAliasDeclaration, builtin_data_type::ParsedBuiltinTypeDeclaration,
            module::ParsedModuleDeclaration, r#struct::ParsedStructDeclaration,
        },
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    },
    span::Span,
    typed::{
        data_type::unresolved::SemanticDataType,
        environment::{
            r#type::{
                HighGenericId, HighTypeId, SemanticTypeDeclaration, SemanticTypeDeclarationKind,
                r#struct::HighStructId,
            },
            value::HighValueId,
        },
    },
    visibility::Visibility,
};

pub mod alias;
pub mod builtin_data_type;
pub mod module;
pub mod r#struct;

#[derive(Debug, Clone)]
pub enum ParsedTypeDeclarationKind {
    Module(ParsedModuleDeclaration),
    Struct(ParsedStructDeclaration),
    Alias(ParsedTypeAliasDeclaration),
    Generic(String),
    Builtin(ParsedBuiltinTypeDeclaration),
}

impl From<SemanticTypeDeclarationKind> for ParsedTypeDeclarationKind {
    fn from(value: SemanticTypeDeclarationKind) -> Self {
        match value {
            SemanticTypeDeclarationKind::Module(declaration) => Self::Module(declaration.into()),
            SemanticTypeDeclarationKind::Struct(declaration) => Self::Struct(declaration.into()),
            SemanticTypeDeclarationKind::Alias(declaration) => Self::Alias(declaration.into()),
            SemanticTypeDeclarationKind::Generic(name) => Self::Generic(name),
            SemanticTypeDeclarationKind::Builtin(declaration) => Self::Builtin(declaration.into()),
        }
    }
}

impl ParsedTypeDeclarationKind {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Module(declaration) => &declaration.name,
            Self::Struct(declaration) => declaration.name(),
            Self::Alias(declaration) => &declaration.name,
            Self::Generic(name) => name,
            Self::Builtin(data_type) => &data_type.name,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedTypeDeclaration {
    pub module_path: Vec<String>,
    pub visibility: Visibility,
    pub kind: ParsedTypeDeclarationKind,
}

impl From<SemanticTypeDeclaration> for ParsedTypeDeclaration {
    fn from(value: SemanticTypeDeclaration) -> Self {
        Self {
            module_path: value.module_path,
            visibility: value.visibility,
            kind: value.kind.into(),
        }
    }
}

impl ParsedTypeDeclaration {
    #[must_use]
    pub fn is_visible(&self, current_module_path: &[String]) -> bool {
        if matches!(self.visibility, Visibility::Public) {
            return true;
        }

        current_module_path.starts_with(&self.module_path)
    }

    pub fn get_visible_type_id(
        &self,
        ctx: &SemanticAnalysisContext,
        id: HighTypeId,
        name: &str,
    ) -> Result<HighTypeId, SemanticAnalysisError> {
        match &self.kind {
            ParsedTypeDeclarationKind::Module(declaration) => {
                declaration.get_type_id_semantic_analysis(name)
            }
            ParsedTypeDeclarationKind::Struct(declaration) => {
                if let Some(impls) = ctx.semantic_environment.impls.get(&id) {
                    for implementation in impls {
                        if let Some(id) = implementation.types.get(name) {
                            return Ok(*id);
                        }
                    }
                }

                Err(SemanticAnalysisError::TypeDoesntContainType {
                    container_type_name: declaration.name().to_owned(),
                    type_name: name.to_owned(),
                })
            }
            _ => Err(SemanticAnalysisError::TypeDoesntContainItems {
                type_name: self.kind.name().to_owned(),
            }),
        }
    }

    pub fn get_visible_value_id(
        &self,
        ctx: &SemanticAnalysisContext,
        id: HighTypeId,
        name: &str,
    ) -> Result<HighValueId, SemanticAnalysisError> {
        match &self.kind {
            ParsedTypeDeclarationKind::Module(declaration) => {
                declaration.get_value_id_semantic_analysis(name)
            }
            ParsedTypeDeclarationKind::Struct(declaration) => {
                declaration.get_value_id_semantic_analysis(ctx, id, name)
            }
            _ => Err(SemanticAnalysisError::TypeDoesntContainItems {
                type_name: self.kind.name().to_owned(),
            }),
        }
    }

    pub fn resolve_partially(
        self,
        ctx: &mut SemanticAnalysisContext,
        id: HighTypeId,
        generic_spans: Vec<Span>,
        generic_types: Vec<SemanticDataType>,
        path_span: Span,
    ) -> SemanticDataType {
        match self.kind {
            ParsedTypeDeclarationKind::Module(ParsedModuleDeclaration { name, .. }) => {
                ctx.add_error_type(path_span, SemanticAnalysisError::NotAType(name))
            }
            ParsedTypeDeclarationKind::Struct(declaration) => {
                let id = HighStructId(id.0);

                let expected_generics = declaration.generic_count();
                let actual_generics = generic_types.len();

                if actual_generics != expected_generics {
                    return ctx.add_invalid_generics_type(
                        path_span,
                        declaration.name(),
                        expected_generics,
                        actual_generics,
                    );
                }

                SemanticDataType::Struct(id, generic_types)
            }
            ParsedTypeDeclarationKind::Alias(declaration) => {
                let expected_generics = declaration.generic_ids.len();
                let actual_generics = generic_types.len();

                if actual_generics != expected_generics {
                    return ctx.add_invalid_generics_type(
                        path_span,
                        &declaration.name,
                        expected_generics,
                        actual_generics,
                    );
                }

                let resolved_declaration = ctx.semantic_environment.get_type(id);

                let SemanticTypeDeclarationKind::Alias(resolved_alias) = &resolved_declaration.kind
                else {
                    unreachable!()
                };

                resolved_alias
                    .alias
                    .clone()
                    .substitute_generics(&declaration.generic_ids, &generic_types)
            }
            ParsedTypeDeclarationKind::Generic(name) => {
                let expected_generics = 0;
                let actual_generics = generic_types.len();

                if actual_generics != expected_generics {
                    return ctx.add_invalid_generics_type(
                        path_span,
                        &name,
                        expected_generics,
                        actual_generics,
                    );
                }

                SemanticDataType::Generic(HighGenericId(id.0))
            }
            ParsedTypeDeclarationKind::Builtin(data_type) => data_type
                .to_data_type_semantic_analysis(ctx, path_span, generic_spans, generic_types),
        }
    }
}
