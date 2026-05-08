use crate::{
    high::{item::Item, semantic_analysis::SemanticAnalysisContext},
    low::program::Program as MiddleProgram,
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Item>,
}

impl Program {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleProgram> {
        let items = self
            .items
            .into_iter()
            .map(|item| item.perform_semantic_analysis(ctx))
            .collect_option_all()?;

        Some(MiddleProgram { items })
    }
}
