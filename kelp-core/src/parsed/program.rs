use std::collections::HashSet;

use la_arena::Idx;

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    parsed::{
        environment::resolved::{
            SemanticEnvironment,
            value::{
                SemanticValueDeclaration, SemanticValueDeclarationKind,
                function::{
                    HighFunctionId, SemanticFunctionDeclaration,
                    regular::SemanticRegularFunctionDeclaration,
                },
            },
        },
        item::Item,
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    },
    trait_ext::CollectOptionAllIterExt,
    typed::program::Program as MiddleProgram,
};

fn calls_recursively(
    resolved_environment: &SemanticEnvironment,
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

    let (
        _,
        _,
        SemanticFunctionDeclaration::Regular(SemanticRegularFunctionDeclaration { calls, .. }),
    ) = resolved_environment.get_function(call_id)
    else {
        return false;
    };

    for (_, call_id) in calls {
        if calls_recursively(resolved_environment, callee_id, *call_id, visited_calls) {
            return true;
        }
    }

    false
}

#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Idx<Item>>,
}

impl Program {
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleProgram> {
        for item in self.items.iter().copied() {
            Item::resolve_names(item, high_allocator, ctx);
        }

        for item in self.items.iter().copied() {
            Item::resolve_imports(item, high_allocator, ctx);
        }

        for item in self.items.iter().copied() {
            Item::resolve_types(item, high_allocator, ctx);
        }

        for item in self.items.iter().copied() {
            Item::resolve_value_types(item, high_allocator, ctx);
        }

        let items = self
            .items
            .into_iter()
            .map(|item| Item::perform_semantic_analysis(item, high_allocator, low_allocator, ctx))
            .collect_option_all()?;

        let mut failed = false;

        for (callee_id, value) in ctx.semantic_environment.iter_values() {
            let callee_id = HighFunctionId(callee_id.0);

            let SemanticValueDeclaration {
                kind: SemanticValueDeclarationKind::Function(declaration),
                ..
            } = value
            else {
                continue;
            };

            let SemanticFunctionDeclaration::Regular(SemanticRegularFunctionDeclaration {
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
                        &ctx.semantic_environment,
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
