use std::collections::HashSet;

use la_arena::Idx;

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    parsed::{
        data_type::DataType,
        environment::{
            resolved::{r#type::HighGenericId, value::function::regular::HighRegularFunctionId},
            unresolved::{
                r#type::UnresolvedTypeDeclarationKind,
                value::{
                    UnresolvedValueDeclarationKind,
                    function::{
                        UnresolvedFunctionDeclaration,
                        regular::UnresolvedRegularFunctionDeclaration,
                    },
                },
            },
        },
        expression::block::BlockExpression,
        item::Item,
        pattern::Pattern,
        semantic_analysis::{
            FunctionContext, RegularFunctionModifiers, SemanticAnalysisContext,
            info::error::SemanticAnalysisError,
        },
    },
    span::Span,
    typed::{
        data_type::unresolved::UnresolvedDataType, expression::typed::TypedExpression,
        item::Item as MiddleItem,
    },
    visibility::Visibility,
};

#[derive(Debug, Clone)]
pub struct FunctionDeclarationItem {
    pub recursive_keyword_span: Option<Span>,
    pub runtime_keyword_span: Option<Span>,
    pub name_span: Span,
    pub name: String,
    pub generic_names: Vec<String>,
    pub is_method: bool,
    pub parameters: Vec<(Idx<Pattern>, DataType)>,
    pub return_type: DataType,
    pub body: BlockExpression,
}

impl FunctionDeclarationItem {
    pub fn resolve_names(
        &self,
        item_id: Idx<Item>,
        ctx: &mut SemanticAnalysisContext,
        visibility: Visibility,
    ) -> Option<()> {
        if ctx.current_scope().value_is_declared(&self.name) {
            return ctx.add_error(
                self.name_span,
                SemanticAnalysisError::ValueAlreadyDeclared(self.name.clone()),
            );
        }

        let generic_ids = self
            .generic_names
            .iter()
            .cloned()
            .map(|generic_name| {
                let id = ctx.declare_unresolved_type(
                    Visibility::Public,
                    UnresolvedTypeDeclarationKind::Generic(generic_name),
                );

                HighGenericId(id.0)
            })
            .collect::<Vec<_>>();

        ctx.declare_item_generic_ids(item_id, generic_ids.clone());

        let value_id = ctx.declare_unresolved_value(
            visibility,
            UnresolvedValueDeclarationKind::Function(Box::new(
                UnresolvedFunctionDeclaration::Regular(UnresolvedRegularFunctionDeclaration {
                    name: self.name.clone(),
                    generic_ids,
                }),
            )),
        );

        ctx.declare_item_value_id(item_id, value_id);

        Some(())
    }

    pub fn resolve_types(
        &self,
        id: Idx<Item>,
        ctx: &mut SemanticAnalysisContext,
        visibility: Visibility,
    ) {
        let generic_ids = ctx.get_item_generic_ids(id).to_vec();
        let id = ctx.get_item_value_id(id);

        ctx.enter_scope();

        for (generic_id, generic_name) in generic_ids
            .iter()
            .copied()
            .zip(self.generic_names.iter().cloned())
        {
            ctx.declare_generic(generic_id, Visibility::Public, generic_name);
        }

        let parameters = self
            .parameters
            .iter()
            .map(|(_, parameter_type)| {
                let parameter_type = parameter_type.clone().perform_semantic_analysis(ctx);

                (None, parameter_type)
            })
            .collect::<Vec<_>>();

        let return_type = self.return_type.clone().perform_semantic_analysis(ctx);

        ctx.exit_scope();

        let modifiers = if self.runtime_keyword_span.is_some() {
            RegularFunctionModifiers::Runtime {
                recursive: self.recursive_keyword_span.is_some(),
            }
        } else {
            RegularFunctionModifiers::None
        };

        ctx.declare_regular_function(
            id,
            visibility,
            modifiers,
            self.name.clone(),
            generic_ids,
            parameters,
            return_type,
        );
    }

    #[must_use]
    pub fn perform_semantic_analysis(
        &self,
        id: Idx<Item>,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<Idx<MiddleItem>> {
        let id = ctx.get_item_value_id(id);

        ctx.enter_scope();

        let parameter_types = self
            .parameters
            .iter()
            .map(|(_, parameter_type)| parameter_type.clone().perform_semantic_analysis(ctx))
            .collect::<Vec<_>>();

        let return_type = self.return_type.clone().perform_semantic_analysis(ctx);

        let mut failed = false;

        if let Some(runtime_keyword_span) = self.runtime_keyword_span {
            let all_types_are_runtime = {
                let parameter_types_are_runtime = parameter_types
                    .iter()
                    .all(|parameter_type| !matches!(parameter_type.is_compiletime(), Some(true)));

                let return_type_is_runtime = !matches!(return_type.is_compiletime(), Some(true));

                parameter_types_are_runtime && return_type_is_runtime
            };

            if !all_types_are_runtime {
                ctx.add_error_unit(
                    runtime_keyword_span,
                    SemanticAnalysisError::FunctionTypesNotAllRuntime,
                );

                failed = true;
            }

            if let Some(recursive_keyword_span) = self.recursive_keyword_span {
                let all_types_are_data = {
                    let parameter_types_valid = parameter_types.iter().all(|parameter_type| {
                        matches!(parameter_type, UnresolvedDataType::Data(..))
                    });

                    let return_type_valid = matches!(return_type, UnresolvedDataType::Data(..));

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
        } else if let Some(recursive_keyword_span) = self.recursive_keyword_span {
            ctx.add_error_unit(
                recursive_keyword_span,
                SemanticAnalysisError::RecursiveFunctionNotRuntime,
            );

            failed = true;
        }

        let modifiers = if self.runtime_keyword_span.is_some() {
            RegularFunctionModifiers::Runtime {
                recursive: self.recursive_keyword_span.is_some(),
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
            calls: HashSet::new(),
        });

        let mut resolved_parameters = Vec::with_capacity(self.parameters.len());
        let mut resolved_all_parameters = true;

        for ((pattern, _), data_type) in self.parameters.iter().zip(parameter_types) {
            let Some(resolved_pattern) = Pattern::perform_semantic_analysis(
                *pattern,
                high_allocator,
                low_allocator,
                ctx,
                &data_type,
            ) else {
                resolved_all_parameters = false;

                continue;
            };

            if resolved_all_parameters {
                resolved_parameters.push((resolved_pattern, data_type));
            }
        }

        let id = HighRegularFunctionId(id.0);

        let after_body_analysis = |ctx: &mut SemanticAnalysisContext| {
            let context = ctx.function_contexts.pop().unwrap();

            ctx.exit_scope();

            ctx.resolved_environment
                .update_regular_function(id, |declaration| {
                    if let FunctionContext::Regular { calls, .. } = &context {
                        declaration.calls.clone_from(calls);
                    }
                });

            context
        };

        let Some((body_span, tail_expression_span, body)) =
            self.body
                .perform_semantic_analysis(high_allocator, low_allocator, ctx)
        else {
            after_body_analysis(ctx);

            return None;
        };

        let context = after_body_analysis(ctx);

        if !TypedExpression::definitely_diverges(body, low_allocator) {
            let body_type = low_allocator.get_expression_type(body);

            body_type.assert_equals(
                ctx,
                tail_expression_span.unwrap_or(body_span),
                context.return_type(),
            )?;
        }

        if resolved_all_parameters {
            ctx.resolved_environment
                .update_regular_function(id, |declaration| {
                    declaration.parameters = resolved_parameters
                        .into_iter()
                        .map(|(pattern, data_type)| (Some(pattern), data_type))
                        .collect();

                    declaration.body = Some(body);
                });
        } else {
            return None;
        }

        Some(low_allocator.allocate_item(MiddleItem::FunctionDeclaration))
    }
}
