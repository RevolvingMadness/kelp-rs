use std::collections::BTreeMap;

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    compile_context::CompileContext,
    data_type::high::HighDataType,
    datapack::{DataTypeDeclarationKind, HighDatapack},
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
    span::Span,
    statement::Statement,
    trait_ext::OptionUnitIterExt,
};

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    MCFNDeclaration(ResourceLocation, Box<Statement>),
    TypeAliasDeclaration(String, Vec<String>, HighDataType),
    StructDeclaration(String, Vec<String>, BTreeMap<String, HighDataType>),
}

impl ItemKind {
    #[must_use]
    pub const fn with_span(self, span: Span) -> Item {
        Item { span, kind: self }
    }

    pub fn compile(self, datapack: &mut HighDatapack, _ctx: &mut CompileContext) {
        match self {
            Self::MCFNDeclaration(id, statement) => {
                datapack.within_namespace(id.namespace(), |datapack| {
                    datapack.push_function_to_current_namespace(id.paths.clone());

                    let mut function_ctx = CompileContext::default();

                    statement.kind.compile(datapack, &mut function_ctx);

                    let function_commands = function_ctx.compile();

                    datapack
                        .current_namespace_mut()
                        .current_function_mut()
                        .add_commands(function_commands);

                    datapack.pop_function_from_current_namespace();
                });
            }
            Self::TypeAliasDeclaration(name, generics, alias) => {
                let alias = alias.kind.resolve(datapack, Some(&generics)).unwrap();

                datapack.declare_data_type(
                    name.clone(),
                    DataTypeDeclarationKind::Alias {
                        name,
                        generics,
                        alias,
                    },
                );
            }
            Self::StructDeclaration(name, generics, fields) => {
                let resolved_fields = fields
                    .into_iter()
                    .map(|(key, data_type)| {
                        (
                            key,
                            data_type.kind.resolve(datapack, Some(&generics)).unwrap(),
                        )
                    })
                    .collect();

                datapack.declare_data_type(
                    name.clone(),
                    DataTypeDeclarationKind::Struct {
                        name,
                        generics,
                        fields: resolved_fields,
                    },
                );
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub span: Span,
    pub kind: ItemKind,
}

impl Item {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match &self.kind {
            ItemKind::MCFNDeclaration(_, statement) => {
                statement.perform_semantic_analysis(ctx, is_lhs)
            }
            ItemKind::TypeAliasDeclaration(name, generic_names, alias) => {
                if ctx.data_type_is_declared(name) {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::TypeIsAlreadyDefined(name.clone()),
                        ),
                    });
                }

                if alias
                    .perform_semantic_analysis(Some(generic_names), ctx)
                    .is_some()
                {
                    let alias = alias.kind.resolve(ctx, Some(generic_names)).unwrap();

                    ctx.declare_data_type(
                        name.clone(),
                        Some(DataTypeDeclarationKind::Alias {
                            name: name.clone(),
                            generics: generic_names.clone(),
                            alias,
                        }),
                    );

                    Some(())
                } else {
                    ctx.declare_data_type(name.clone(), None);

                    None
                }
            }
            ItemKind::StructDeclaration(name, generic_names, field_types) => {
                if ctx.data_type_is_declared(name) {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::TypeIsAlreadyDefined(name.clone()),
                        ),
                    });
                }

                if field_types
                    .values()
                    .map(|field| field.perform_semantic_analysis(Some(generic_names), ctx))
                    .all_some()
                    .is_none()
                {
                    ctx.declare_data_type(name.clone(), None);

                    return None;
                }

                let resolved_fields = field_types
                    .iter()
                    .map(|(key, field)| {
                        field
                            .kind
                            .resolve(ctx, Some(generic_names))
                            .map(|data_type| (key.clone(), data_type))
                    })
                    .collect::<Option<BTreeMap<_, _>>>()?;

                ctx.declare_data_type(
                    name.clone(),
                    Some(DataTypeDeclarationKind::Struct {
                        name: name.clone(),
                        generics: generic_names.clone(),
                        fields: resolved_fields,
                    }),
                );

                Some(())
            }
        }
    }
}
