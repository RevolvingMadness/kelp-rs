use ordered_float::NotNan;

use crate::{
    cst_node,
    semantic_token::{SemanticToken, SemanticTokenType},
    syntax::SyntaxKind,
};

cst_node!(CSTLocalCoordinate, SyntaxKind::LocalCoordinate);

impl<'a> CSTLocalCoordinate<'a> {
    pub fn lower(self, text: &str) -> Option<NotNan<f32>> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::FractionalValue {
                Some(token.text(text).parse().unwrap())
            } else {
                None
            }
        })
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        for token in self.0.children_tokens() {
            match token.kind {
                SyntaxKind::Caret => {
                    tokens.push(SemanticToken::new(token.span, SemanticTokenType::Function));
                }

                SyntaxKind::FractionalValue | SyntaxKind::WholeValue => {
                    tokens.push(SemanticToken::new(token.span, SemanticTokenType::Number));
                }

                _ => {}
            }
        }
    }
}

cst_node!(CSTLocalCoordinates, SyntaxKind::LocalCoordinates);

impl<'a> CSTLocalCoordinates<'a> {
    pub fn coordinates(
        self,
    ) -> (
        Option<CSTLocalCoordinate<'a>>,
        Option<CSTLocalCoordinate<'a>>,
        Option<CSTLocalCoordinate<'a>>,
    ) {
        let mut coordinates = self.children().filter_map(CSTLocalCoordinate::cast);

        let x = coordinates.next();
        let y = coordinates.next();
        let z = coordinates.next();

        (x, y, z)
    }
}
