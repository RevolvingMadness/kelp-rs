use kelp_core::span::Span;

use crate::{
    cst_node,
    semantic_token::{SemanticToken, SemanticTokenType},
    syntax::SyntaxKind,
};

cst_node!(
    CSTVariableEntitySelector,
    SyntaxKind::VariableEntitySelector
);

impl<'a> CSTVariableEntitySelector<'a> {
    pub fn variable_span(&self) -> Option<Span> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::EntitySelectorVariable {
                Some(token.span)
            } else {
                None
            }
        })
    }

    pub fn variable<'b>(&self, text: &'b str) -> Option<&'b str> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::EntitySelectorVariable {
                Some(token.text(text))
            } else {
                None
            }
        })
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
