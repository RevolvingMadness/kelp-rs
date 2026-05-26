use std::collections::HashSet;

use la_arena::Idx;

use crate::{
    parsed::arena::ParsedAstArena,
    parsed::{
        item::ParsedItem,
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    },
    trait_ext::CollectOptionAllIterExt,
    typed::arena::TypedAstArena,
    typed::{
        environment::{
            SemanticEnvironment,
            value::{
                SemanticValueDeclaration, SemanticValueDeclarationKind,
                function::{
                    HighFunctionId, SemanticFunctionDeclaration,
                    regular::SemanticRegularFunctionDeclaration,
                },
            },
        },
        program::Program as MiddleProgram,
    },
};

fn calls_recursively(
    semantic_environment: &SemanticEnvironment,
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
    ) = semantic_environment.get_function(call_id)
    else {
        return false;
    };

    for (_, call_id) in calls {
        if calls_recursively(semantic_environment, callee_id, *call_id, visited_calls) {
            return true;
        }
    }

    false
}

#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Idx<ParsedItem>>,
}

impl Program {
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleProgram> {
        for item in self.items.iter().copied() {
            ParsedItem::resolve_names(item, parsed_arena, ctx);
        }

        for item in self.items.iter().copied() {
            ParsedItem::resolve_imports(item, parsed_arena, ctx);
        }

        for item in self.items.iter().copied() {
            ParsedItem::resolve_types(item, parsed_arena, ctx);
        }

        for item in self.items.iter().copied() {
            ParsedItem::resolve_value_types(item, parsed_arena, ctx);
        }

        let items = self
            .items
            .into_iter()
            .map(|item| ParsedItem::perform_semantic_analysis(item, parsed_arena, typed_arena, ctx))
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
