use kelp_core::{high::entity_selector::HighEntitySelector, span::Span};
use minecraft_command_types::entity_selector::EntitySelectorVariable;

use crate::{
    cst_node,
    semantic_token::{SemanticToken, SemanticTokenType},
    syntax::SyntaxKind,
};

cst_node!(
    CSTVariableEntitySelector,
    SyntaxKind::VariableEntitySelector
);

impl CSTVariableEntitySelector<'_> {
    #[must_use]
    pub fn variable_span(&self) -> Option<Span> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::EntitySelectorVariable {
                Some(token.span)
            } else {
                None
            }
        })
    }

    #[must_use]
    pub fn variable<'b>(&self, text: &'b str) -> Option<&'b str> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::EntitySelectorVariable {
                Some(token.text(text))
            } else {
                None
            }
        })
    }

    #[must_use]
    pub fn lower(self, text: &str) -> Option<HighEntitySelector> {
        let variable = self.variable(text)?;

        let variable = match variable {
            "a" => EntitySelectorVariable::A,
            "e" => EntitySelectorVariable::E,
            "n" => EntitySelectorVariable::N,
            "p" => EntitySelectorVariable::P,
            "r" => EntitySelectorVariable::R,
            _ => EntitySelectorVariable::S,
        };

        Some(HighEntitySelector::Variable(variable, Vec::new()))
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        if let Some(variable_span) = self.variable_span() {
            tokens.push(SemanticToken::new(
                Span {
                    start: self.span().start,
                    end: variable_span.end,
                }, // TODO: Maybe a better way to do this?
                SemanticTokenType::Variable,
            ));
        }

        // TODO handle options
    }
}
