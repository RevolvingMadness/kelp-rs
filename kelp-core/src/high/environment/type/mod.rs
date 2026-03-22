use crate::{
    builtin_data_type::BuiltinDataType,
    high::{
        data_type::resolved::{GenericResolver, PartiallyResolvedDataType},
        environment::r#type::{
            alias::HighAliasDeclaration, module::HighModuleDeclaration,
            r#struct::HighStructDeclaration,
        },
        semantic_analysis_context::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    },
    middle::data_type::DataType,
    span::Span,
};

pub mod alias;
pub mod module;
pub mod r#struct;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighTypeId(pub usize);

#[derive(Debug, Clone)]
pub enum HighTypeDeclaration {
    Module(HighModuleDeclaration),
    Struct(HighStructDeclaration),
    Alias(HighAliasDeclaration),
    Builtin(BuiltinDataType),
}

impl HighTypeDeclaration {
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

    pub fn resolve_fully(
        self,
        ctx: &mut SemanticAnalysisContext,
        id: HighTypeId,
        generic_types: Vec<DataType>,
        path_span: Span,
    ) -> Option<DataType> {
        match self {
            Self::Module(_) => ctx.add_error(
                path_span,
                SemanticAnalysisError::NotAType(self.name().to_owned()),
            ),
            Self::Struct(declaration) => {
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
                                let field_type = field_type?.resolve_fully(ctx, &resolver).unwrap();

                                Some((field_name, field_type))
                            })
                            .collect::<Option<_>>()?;

                        ctx.declare_monomorphized_struct_struct(
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
                                let field_type = field_type?.resolve_fully(ctx, &resolver).unwrap();

                                Some(field_type)
                            })
                            .collect::<Option<_>>()?;

                        ctx.declare_monomorphized_tuple_struct(
                            id,
                            declaration.name,
                            generic_types,
                            field_types,
                        )
                    }
                };

                Some(DataType::Struct(id))
            }
            Self::Alias(declaration) => {
                let alias = declaration.alias?;

                let resolver = GenericResolver::create_semantic_analysis(
                    ctx,
                    &declaration.name,
                    path_span,
                    &declaration.generic_names,
                    &generic_types,
                )?;

                alias.resolve_fully(ctx, &resolver)
            }
            Self::Builtin(data_type) => data_type.to_data_type(generic_types),
        }
    }

    #[must_use]
    pub fn resolve_partially(
        self,
        id: HighTypeId,
        name_span: Span,
        generic_types: Vec<PartiallyResolvedDataType>,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<PartiallyResolvedDataType> {
        if let Some(expected_generic_count) = self.generic_count() {
            let actual_generic_count = generic_types.len();

            if actual_generic_count != expected_generic_count {
                let name = self.name().to_owned();

                return ctx.add_invalid_generics(
                    name_span,
                    name,
                    expected_generic_count,
                    actual_generic_count,
                );
            }
        }

        match self {
            Self::Module(_) => ctx.add_error(
                name_span,
                SemanticAnalysisError::NotAType(self.name().to_owned()),
            ),
            Self::Struct(_) => Some(PartiallyResolvedDataType::Struct(
                name_span,
                id,
                generic_types,
            )),
            Self::Alias(declaration) => declaration.alias,
            Self::Builtin(data_type) => data_type.to_resolved_data_type(generic_types),
        }
    }
}
