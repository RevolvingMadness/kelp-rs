use std::collections::HashSet;

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    parsed::{
        expression::block::ParsedBlockExpression,
        pattern::ParsedPattern,
        semantic_analysis::{
            FunctionContext, RegularFunctionModifiers, SemanticAnalysisContext,
            info::error::SemanticAnalysisError, scope::Scope,
        },
    },
    semantic::{
        data_type::SemanticDataType,
        environment::{
            r#type::{
                HighGenericId, HighTypeId, SemanticTypeDeclarationKind,
                alias::SemanticTypeAliasDeclaration,
            },
            value::function::regular::HighRegularFunctionId,
        },
        item::SemanticItem,
    },
    span::Span,
    trait_ext::CollectOptionAllIterExt,
    visibility::Visibility,
};

#[derive(Debug, Clone)]
pub enum TypedItem {
    InherentImplementationItem {
        generic_names: Vec<String>,
        target_type_span: Span,
        target_type: SemanticDataType,
        associated_items: Vec<Self>,

        self_type_id: HighTypeId,
        generic_ids: Vec<HighGenericId>,
        associated_items_scope: Scope,
    },
    ModuleDeclaration {
        name: String,
        items: Vec<Self>,
    },
    FunctionDeclaration {
        recursive_keyword_span: Option<Span>,
        runtime_keyword_span: Option<Span>,
        generic_ids: Vec<HighGenericId>,
        generic_names: Vec<String>,
        parameters: Vec<(ParsedPattern, SemanticDataType)>,
        return_type: SemanticDataType,
        body: Box<ParsedBlockExpression>,

        id: HighRegularFunctionId,
    },
    MinecraftFunctionDeclaration {
        resource_location: ResourceLocation,
        body: Box<ParsedBlockExpression>,
    },
    TypeAliasDeclaration,
    RegularStructDeclaration,
    TupleStructDeclaration,
    Use,
}

impl TypedItem {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticItem> {
        Some(match self {
            Self::InherentImplementationItem {
                generic_names,
                target_type,
                associated_items,
                self_type_id,
                generic_ids,
                associated_items_scope,
                ..
            } => {
                ctx.enter_scope();

                for (generic_id, generic_name) in generic_ids
                    .iter()
                    .copied()
                    .zip(generic_names.iter().cloned())
                {
                    ctx.set_semantic_generic(generic_id, Visibility::Public, generic_name);
                }

                ctx.set_semantic_type(
                    self_type_id,
                    Visibility::Public,
                    SemanticTypeDeclarationKind::Alias(SemanticTypeAliasDeclaration {
                        name: "Self".to_owned(),
                        generic_ids: Vec::new(),
                        alias: target_type,
                    }),
                );

                ctx.push_scope(associated_items_scope);

                let _associated_items = associated_items
                    .into_iter()
                    .map(|item| item.perform_semantic_analysis(ctx))
                    .collect_option_all::<Vec<_>>();

                ctx.exit_scope();

                ctx.exit_scope();

                SemanticItem::InherentImplementation
            }
            Self::ModuleDeclaration { name, items } => {
                let mut failed = false;

                ctx.enter_module(name);

                for item in items {
                    if item.perform_semantic_analysis(ctx).is_none() {
                        failed = true;
                    }
                }

                ctx.exit_module();

                if failed {
                    return None;
                }

                SemanticItem::ModuleDeclaration
            }
            Self::FunctionDeclaration {
                recursive_keyword_span,
                runtime_keyword_span,
                generic_ids,
                generic_names,
                parameters,
                return_type,
                body,
                id,
            } => {
                ctx.enter_scope();

                for (generic_id, generic_name) in generic_ids
                    .iter()
                    .copied()
                    .zip(generic_names.iter().cloned())
                {
                    ctx.set_semantic_generic(generic_id, Visibility::Public, generic_name);
                }

                let mut failed = false;

                if let Some(runtime_keyword_span) = runtime_keyword_span {
                    let all_types_are_runtime = {
                        let parameter_types_are_runtime =
                            parameters.iter().all(|(_, parameter_type)| {
                                !matches!(parameter_type.is_compiletime(), Some(true))
                            });

                        let return_type_is_runtime =
                            !matches!(return_type.is_compiletime(), Some(true));

                        parameter_types_are_runtime && return_type_is_runtime
                    };

                    if !all_types_are_runtime {
                        ctx.add_error_unit(
                            runtime_keyword_span,
                            SemanticAnalysisError::FunctionTypesNotAllRuntime,
                        );

                        failed = true;
                    }

                    if let Some(recursive_keyword_span) = recursive_keyword_span {
                        let all_types_are_data = {
                            let parameter_types_valid =
                                parameters.iter().all(|(_, parameter_type)| {
                                    matches!(parameter_type, SemanticDataType::Data(..))
                                });

                            let return_type_valid =
                                matches!(return_type, SemanticDataType::Data(..));

                            parameter_types_valid && return_type_valid
                        };

                        if !all_types_are_data {
                            ctx.add_error_unit(
                                recursive_keyword_span,
                                SemanticAnalysisError::FunctionTypesNotAllData,
                            );

                            failed = true;
                        }
                    }
                } else if let Some(recursive_keyword_span) = recursive_keyword_span {
                    ctx.add_error_unit(
                        recursive_keyword_span,
                        SemanticAnalysisError::RecursiveFunctionNotRuntime,
                    );

                    failed = true;
                }

                let modifiers = if runtime_keyword_span.is_some() {
                    RegularFunctionModifiers::Runtime {
                        recursive: recursive_keyword_span.is_some(),
                    }
                } else {
                    RegularFunctionModifiers::None
                };

                let scope = ctx.exit_scope();

                if failed {
                    return None;
                }

                ctx.push_scope(scope);

                ctx.function_contexts.push(FunctionContext::Regular {
                    modifiers,
                    return_type,
                    callee_id: id,
                    calls: HashSet::new(),
                });

                let mut resolved_parameters = Vec::with_capacity(parameters.len());
                let mut resolved_all_parameters = true;

                for (pattern, data_type) in parameters {
                    let Some(pattern) = pattern.perform_semantic_analysis(ctx, &data_type) else {
                        resolved_all_parameters = false;

                        continue;
                    };

                    if resolved_all_parameters {
                        resolved_parameters.push((pattern, data_type));
                    }
                }

                let after_body_analysis = |ctx: &mut SemanticAnalysisContext| {
                    let context = ctx.function_contexts.pop().unwrap();

                    ctx.exit_scope();

                    ctx.semantic_environment
                        .update_regular_function(id, |declaration| {
                            if let FunctionContext::Regular { calls, .. } = &context {
                                declaration.calls.clone_from(calls);
                            }
                        });

                    context
                };

                let Some((body_span, tail_expression_span, body)) =
                    body.perform_semantic_analysis(ctx)
                else {
                    after_body_analysis(ctx);

                    return None;
                };

                let context = after_body_analysis(ctx);

                if !body.kind.definitely_diverges() {
                    body.data_type.assert_equals(
                        ctx,
                        tail_expression_span.unwrap_or(body_span),
                        context.return_type(),
                    )?;
                }

                if resolved_all_parameters {
                    ctx.semantic_environment
                        .update_regular_function(id, |declaration| {
                            declaration.parameters = resolved_parameters
                                .into_iter()
                                .map(|(pattern, data_type)| (Some(pattern), data_type))
                                .collect();
                            declaration.body = Some(Box::new(body));
                        });
                } else {
                    return None;
                }

                SemanticItem::FunctionDeclaration
            }
            Self::MinecraftFunctionDeclaration {
                resource_location,
                body,
            } => {
                ctx.function_contexts.push(FunctionContext::MCFunction);

                let (body_span, tail_expression_span, body) =
                    body.perform_semantic_analysis(ctx)?;

                ctx.function_contexts.pop();

                body.data_type.assert_equals(
                    ctx,
                    tail_expression_span.unwrap_or(body_span),
                    &SemanticDataType::Unit,
                )?;

                SemanticItem::MinecraftFunctionDeclaration(resource_location, body)
            }
            Self::TypeAliasDeclaration => SemanticItem::TypeAliasDeclaration,
            Self::RegularStructDeclaration => SemanticItem::RegularStructDeclaration,
            Self::TupleStructDeclaration => SemanticItem::TupleStructDeclaration,
            Self::Use => SemanticItem::Use,
        })
    }
}
