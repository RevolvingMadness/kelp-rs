use std::collections::HashSet;

use crate::{
    high::{
        environment::{
            HighEnvironment,
            value::{
                HighValueDeclaration, HighValueDeclarationKind,
                function::{
                    HighFunctionDeclaration, HighFunctionId,
                    regular::HighRegularFunctionDeclaration,
                },
            },
        },
        item::Item,
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    },
    low::program::Program as MiddleProgram,
    trait_ext::CollectOptionAllIterExt,
};

fn calls_recursively(
    high_environment: &HighEnvironment,
    callee_id: HighFunctionId,
    call_id: HighFunctionId,
    visited_calls: &mut HashSet<HighFunctionId>,
) -> bool {
    if call_id == callee_id {
        return true;
    }

    if !visited_calls.insert(call_id) {
        return false;
    }

    let (_, _, HighFunctionDeclaration::Regular(HighRegularFunctionDeclaration { calls, .. })) =
        high_environment.get_function(call_id)
    else {
        return false;
    };

    for (_, call_id) in calls {
        if calls_recursively(high_environment, callee_id, *call_id, visited_calls) {
            return true;
        }
    }

    false
}

#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Item>,
}

impl Program {
    pub fn perform_semantic_analysis(
        mut self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleProgram> {
        for item in &mut self.items {
            item.resolve_names(ctx);
        }

        for item in &mut self.items {
            item.resolve_imports(ctx);
        }

        for item in &mut self.items {
            item.resolve_types(ctx);
        }

        let items = self
            .items
            .into_iter()
            .map(|item| item.perform_semantic_analysis(ctx))
            .collect_option_all()?;

        let mut failed = false;

        for (callee_id, value) in ctx.high_environment.iter_values() {
            let callee_id = HighFunctionId(callee_id.0);

            let HighValueDeclaration {
                kind: HighValueDeclarationKind::Function(declaration),
                ..
            } = value
            else {
                continue;
            };

            let HighFunctionDeclaration::Regular(HighRegularFunctionDeclaration {
                modifiers,
                calls,
                ..
            }) = &**declaration
            else {
                continue;
            };

            if !modifiers.is_recursive() {
                for (call_span, call_id) in calls {
                    let mut visited_calls = HashSet::new();

                    if calls_recursively(
                        &ctx.high_environment,
                        callee_id,
                        *call_id,
                        &mut visited_calls,
                    ) {
                        SemanticAnalysisContext::add_error_unit_static(
                            &mut ctx.infos,
                            ctx.max_infos,
                            *call_span,
                            SemanticAnalysisError::RecursiveFunctionCall,
                        );

                        failed = true;
                    }
                }
            }
        }

        (!failed).then_some(MiddleProgram { items })
    }
}
