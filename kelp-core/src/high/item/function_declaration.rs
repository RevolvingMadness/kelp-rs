use std::collections::HashSet;

use crate::{
    high::{
        data_type::DataType,
        environment::r#type::HighTypeDeclarationKind,
        expression::block::BlockExpression,
        pattern::Pattern,
        semantic_analysis::{
            FunctionContext, RegularFunctionModifiers, SemanticAnalysisContext,
            info::error::SemanticAnalysisError,
        },
    },
    low::{data_type::unresolved::UnresolvedDataType, item::Item},
    span::Span,
    visibility::Visibility,
};

#[derive(Debug, Clone)]
pub struct FunctionDeclarationItem {
    pub recursive_keyword_span: Option<Span>,
    pub runtime_keyword_span: Option<Span>,
    pub name_span: Span,
    pub name: String,
    pub generic_names: Vec<String>,
    pub parameters: Vec<(Pattern, DataType)>,
    pub return_type: DataType,
    pub body: BlockExpression,
}

impl FunctionDeclarationItem {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        visibility: Visibility,
    ) -> Option<Item> {
        ctx.enter_scope();

        for generic_name in self.generic_names.clone() {
            ctx.declare_type(
                Visibility::Public,
                HighTypeDeclarationKind::Generic(generic_name),
            );
        }

        let parameter_types = self
            .parameters
            .iter()
            .map(|(_, data_type)| data_type.clone().perform_semantic_analysis(ctx))
            .collect::<Vec<_>>();

        let return_type = self.return_type.perform_semantic_analysis(ctx);

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

        let id = ctx.declare_regular_function(
            visibility,
            modifiers,
            self.name,
            self.generic_names,
            parameter_types
                .iter()
                .cloned()
                .map(|parameter_type| (None, parameter_type))
                .collect(),
            return_type.clone(),
        );

        if failed {
            return None;
        }

        ctx.push_scope(scope);

        let mut calls = HashSet::new();

        calls.insert(id.into());

        ctx.function_contexts.push(FunctionContext::Regular {
            modifiers,
            return_type,
            calls,
        });

        let mut resolved_parameters = Vec::with_capacity(self.parameters.len());
        let mut resolved_all_parameters = true;

        for ((pattern, _), data_type) in
            self.parameters.into_iter().zip(parameter_types.into_iter())
        {
            let Some(resolved_pattern) = pattern.perform_semantic_analysis(ctx, &data_type) else {
                resolved_all_parameters = false;

                continue;
            };

            if resolved_all_parameters {
                resolved_parameters.push((resolved_pattern, data_type));
            }
        }

        let after_body_analysis = |ctx: &mut SemanticAnalysisContext| {
            let context = ctx.function_contexts.pop().unwrap();

            ctx.exit_scope();

            ctx.high_environment
                .update_regular_function(id, |declaration| {
                    if let Some(calls) = context.get_calls() {
                        declaration.calls = Some(calls.clone());
                    }
                });

            context
        };

        let Some((body_span, tail_expression_span, body)) =
            self.body.perform_semantic_analysis(ctx)
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
            ctx.high_environment
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

        Some(Item::FunctionDeclaration)
    }
}
