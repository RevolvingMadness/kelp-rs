use crate::{
    builtin_data_type::BuiltinDataType,
    high::{
        data_type::resolved::GenericResolver,
        environment::r#type::{
            alias::HighAliasDeclaration, module::HighModuleDeclaration,
            r#struct::HighStructDeclaration,
        },
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    },
    low::data_type::DataType,
    span::Span,
    visibility::Visibility,
};

pub mod alias;
pub mod module;
pub mod r#struct;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighTypeId(pub usize);

#[derive(Debug, Clone)]
pub enum HighTypeDeclarationKind {
    Module(HighModuleDeclaration),
    Struct(HighStructDeclaration),
    Alias(HighAliasDeclaration),
    Builtin(BuiltinDataType),
}

impl HighTypeDeclarationKind {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Module(declaration) => &declaration.name,
            Self::Struct(declaration) => declaration.name(),
            Self::Alias(declaration) => &declaration.name,
            Self::Builtin(data_type) => data_type.name(),
        }
    }

    #[must_use]
    pub const fn generic_count(&self) -> Option<usize> {
        Some(match self {
            Self::Module(_) => return None,
            Self::Struct(declaration) => declaration.generic_count(),
            Self::Alias(declaration) => declaration.generic_names.len(),
            Self::Builtin(builtin_type) => builtin_type.generic_count(),
        })
    }
}

#[derive(Debug, Clone)]
pub struct HighTypeDeclaration {
    pub visibility: Visibility,
    pub module_path: Vec<String>,
    pub kind: HighTypeDeclarationKind,
}

impl HighTypeDeclaration {
    #[inline]
    #[must_use]
    pub fn as_tuple_owned(self) -> (Visibility, Vec<String>, HighTypeDeclarationKind) {
        (self.visibility, self.module_path, self.kind)
    }

    pub fn resolve_fully(
        self,
        ctx: &mut SemanticAnalysisContext,
        id: HighTypeId,
        generic_types: Vec<DataType>,
        path_span: Span,
    ) -> Option<DataType> {
        match self.kind {
            HighTypeDeclarationKind::Module(_) => ctx.add_error(
                path_span,
                SemanticAnalysisError::NotAType(self.kind.name().to_owned()),
            ),
            HighTypeDeclarationKind::Struct(declaration) => {
                if let Some(id) = ctx.get_monomorphized_struct_id(id, &generic_types) {
                    return Some(DataType::Struct(id));
                }

                let id = match declaration {
                    HighStructDeclaration::Struct(declaration) => {
                        let resolver = GenericResolver::create_semantic_analysis(
                            ctx,
                            &declaration.name,
                            path_span,
                            &declaration.generic_names,
                            &generic_types,
                        )?;

                        let field_types = declaration
                            .field_types
                            .into_iter()
                            .map(|(field_name, field_type)| {
                                let field_type = field_type?.resolve_fully(&resolver).unwrap();

                                Some((field_name, field_type))
                            })
                            .collect::<Option<_>>()?;

                        ctx.declare_monomorphized_struct_struct(
                            self.visibility,
                            id,
                            declaration.name,
                            generic_types,
                            field_types,
                        )
                    }
                    HighStructDeclaration::Tuple(declaration) => {
                        let resolver = GenericResolver::create_semantic_analysis(
                            ctx,
                            &declaration.name,
                            path_span,
                            &declaration.generic_names,
                            &generic_types,
                        )?;

                        let field_types = declaration
                            .field_types
                            .into_iter()
                            .map(|field_type| {
                                let field_type = field_type?.resolve_fully(&resolver).unwrap();

                                Some(field_type)
                            })
                            .collect::<Option<_>>()?;

                        ctx.declare_monomorphized_tuple_struct(
                            self.visibility,
                            id,
                            declaration.name,
                            generic_types,
                            field_types,
                        )
                    }
                };

                Some(DataType::Struct(id))
            }
            HighTypeDeclarationKind::Alias(declaration) => {
                let resolver = GenericResolver::create_semantic_analysis(
                    ctx,
                    &declaration.name,
                    path_span,
                    &declaration.generic_names,
                    &generic_types,
                )?;

                declaration.alias.resolve_fully(&resolver)
            }
            HighTypeDeclarationKind::Builtin(data_type) => {
                data_type.to_data_type_semantic_analysis(ctx, path_span, generic_types)
            }
        }
    }
}
