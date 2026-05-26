use std::collections::HashSet;

use crate::parsed::environment::r#type::ParsedTypeDeclarationKind;
use crate::parsed::environment::value::{
    ParsedValueDeclarationKind,
    function::{ParsedFunctionDeclaration, regular::ParsedRegularFunctionDeclaration},
};
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::{
    r#type::HighGenericId, value::function::regular::HighRegularFunctionId,
};
use crate::{
    parsed::{
        data_type::ParsedDataType,
        expression::block::ParsedBlockExpression,
        pattern::ParsedPattern,
        semantic_analysis::{
            FunctionContext, RegularFunctionModifiers, SemanticAnalysisContext,
            info::error::SemanticAnalysisError,
        },
    },
    semantic::item::SemanticItem,
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
    pub is_method: bool,
    pub parameters: Vec<(ParsedPattern, ParsedDataType)>,
    pub return_type: ParsedDataType,
    pub body: ParsedBlockExpression,

    pub id: Option<HighRegularFunctionId>,
    pub generic_ids: Option<Vec<HighGenericId>>,
}

impl FunctionDeclarationItem {
    pub fn resolve_names(
        &mut self,
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
                let id = ctx.declare_parsed_type(
                    Visibility::Public,
                    ParsedTypeDeclarationKind::Generic(generic_name),
                );

                HighGenericId(id.0)
            })
            .collect::<Vec<_>>();

        self.generic_ids = Some(generic_ids.clone());

        let id = ctx.declare_parsed_value(
            visibility,
            ParsedValueDeclarationKind::Function(Box::new(ParsedFunctionDeclaration::Regular(
                ParsedRegularFunctionDeclaration {
                    name: self.name.clone(),
                    generic_ids,
                },
            ))),
        );

        self.id = Some(HighRegularFunctionId(id.0));

        Some(())
    }

    pub fn resolve_types(&mut self, ctx: &mut SemanticAnalysisContext, visibility: Visibility) {
        let id = self.id.unwrap();
        let generic_ids = self.generic_ids.as_ref().unwrap();

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
            id.into(),
            visibility,
            modifiers,
            self.name.clone(),
            generic_ids.clone(),
            parameters,
            return_type,
        );
    }

    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticItem> {
        let id = self.id.unwrap();

        ctx.enter_scope();

        let parameter_types = self
            .parameters
            .iter()
            .map(|(_, parameter_type)| parameter_type.clone().perform_semantic_analysis(ctx))
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
                    let parameter_types_valid = parameter_types
                        .iter()
                        .all(|parameter_type| matches!(parameter_type, SemanticDataType::Data(..)));

                    let return_type_valid = matches!(return_type, SemanticDataType::Data(..));

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
            callee_id: id,
            calls: HashSet::new(),
        });

        let mut resolved_parameters = Vec::with_capacity(self.parameters.len());
        let mut resolved_all_parameters = true;

        for ((pattern, _), data_type) in self.parameters.into_iter().zip(parameter_types) {
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

            ctx.semantic_environment
                .update_regular_function(id, |declaration| {
                    if let FunctionContext::Regular { calls, .. } = &context {
                        declaration.calls.clone_from(calls);
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

        Some(SemanticItem::FunctionDeclaration)
    }
}
