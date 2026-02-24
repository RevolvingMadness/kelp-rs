use kelp_core::span::Span;

use crate::{
    cst_node,
    lower::pattern::CSTPattern,
    semantic_token::{SemanticToken, SemanticTokenType},
    syntax::SyntaxKind,
};

cst_node!(CSTStructPatternField, SyntaxKind::StructPatternField);

impl<'a> CSTStructPatternField<'a> {
    pub fn name_span(&self) -> Option<Span> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some(token.span)
            } else {
                None
            }
        })
    }

    pub fn name<'b>(&self, text: &'b str) -> Option<(Span, &'b str)> {
        self.name_span()
            .map(|name_span| (name_span, &text[name_span.into_range()]))
    }

    pub fn pattern(&self) -> Option<CSTPattern<'a>> {
        self.0.children().find_map(CSTPattern::cast)
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        if let Some(name_span) = self.name_span() {
            tokens.push(SemanticToken::new(name_span, SemanticTokenType::Variable));
        }

        if let Some(pattern) = self.pattern() {
            pattern.collect_semantic_tokens(tokens);
        }
    }
}

cst_node!(CSTStructPattern, SyntaxKind::StructPattern);

impl<'a> CSTStructPattern<'a> {
    pub fn name_span(&self) -> Option<Span> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some(token.span)
            } else {
                None
            }
        })
    }

    pub fn name<'b>(&self, text: &'b str) -> Option<&'b str> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some(token.text(text))
            } else {
                None
            }
        })
    }

    pub fn fields(&self) -> Vec<CSTStructPatternField<'a>> {
        self.0
            .children()
            .filter_map(CSTStructPatternField::cast)
            .collect()
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        if let Some(name_span) = self.name_span() {
            tokens.push(SemanticToken::new(name_span, SemanticTokenType::Class));
        }

        for field in self.fields() {
            field.collect_semantic_tokens(tokens);
        }
    }
}
