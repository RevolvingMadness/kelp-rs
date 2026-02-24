use minecraft_command_types::coordinate::WorldCoordinate;
use ordered_float::NotNan;

use crate::{
    cst_node,
    parser::Parser,
    semantic_token::{SemanticToken, SemanticTokenType},
    syntax::SyntaxKind,
};

cst_node!(CSTWorldCoordinate, SyntaxKind::WorldCoordinate);

impl CSTWorldCoordinate<'_> {
    pub fn try_parse(parser: &mut Parser) -> bool {
        let Some(char) = parser.peek_char() else {
            return false;
        };

        let has_symbol = char == '~' || char == '^';

        if !has_symbol && !char.is_numeric() {
            return false;
        }

        parser.start_node(SyntaxKind::WorldCoordinate);
        if has_symbol {
            parser.bump_char();
        }
        parser.try_parse_fractional_value();
        parser.finish_node();

        true
    }

    #[must_use]
    fn is_relative(self) -> bool {
        self.0
            .children_tokens()
            .any(|token| token.kind == SyntaxKind::Tilde)
    }

    #[must_use]
    fn _is_mismatch(self) -> bool {
        self.0
            .children_tokens()
            .any(|token| token.kind == SyntaxKind::Caret)
    }

    #[must_use]
    fn offset(self, text: &str) -> Option<NotNan<f32>> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::FractionalValue {
                Some(token.text(text).parse().unwrap())
            } else {
                None
            }
        })
    }

    #[must_use]
    pub fn lower(self, text: &str) -> Option<WorldCoordinate> {
        let is_relative = self.is_relative();
        let offset = self.offset(text);

        Some(WorldCoordinate {
            relative: is_relative,
            value: offset,
        })
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        for token in self.0.children_tokens() {
            match token.kind {
                SyntaxKind::Tilde => {
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

cst_node!(CSTWorldCoordinates, SyntaxKind::WorldCoordinates);

impl<'a> CSTWorldCoordinates<'a> {
    pub fn coordinates(
        self,
    ) -> (
        Option<CSTWorldCoordinate<'a>>,
        Option<CSTWorldCoordinate<'a>>,
        Option<CSTWorldCoordinate<'a>>,
    ) {
        let mut coordinates = self.children().filter_map(CSTWorldCoordinate::cast);

        let x = coordinates.next();
        let y = coordinates.next();
        let z = coordinates.next();

        (x, y, z)
    }
}
