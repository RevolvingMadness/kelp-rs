use minecraft_command_types::coordinate::WorldCoordinate;
use ordered_float::NotNan;

use crate::{cst_node, parser::Parser, syntax::SyntaxKind};

cst_node!(CSTWorldCoordinate, SyntaxKind::WorldCoordinate);

impl<'a> CSTWorldCoordinate<'a> {
    pub(crate) fn try_parse(parser: &mut Parser) -> bool {
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

    fn is_relative(&self) -> bool {
        self.0
            .children_tokens()
            .any(|token| token.kind == SyntaxKind::Tilde)
    }

    fn is_mismatch(&self) -> bool {
        self.0
            .children_tokens()
            .any(|token| token.kind == SyntaxKind::Caret)
    }

    fn offset(&self) -> Option<NotNan<f32>> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::FractionalValue {
                Some(token.text.parse().unwrap())
            } else {
                None
            }
        })
    }

    pub(crate) fn lower(self) -> Option<WorldCoordinate> {
        let is_relative = self.is_relative();
        let offset = self.offset();

        Some(WorldCoordinate {
            relative: is_relative,
            value: offset,
        })
    }
}

cst_node!(CSTWorldCoordinates, SyntaxKind::WorldCoordinates);

impl<'a> CSTWorldCoordinates<'a> {
    pub(crate) fn coordinates(
        self,
    ) -> Option<(
        CSTWorldCoordinate<'a>,
        CSTWorldCoordinate<'a>,
        CSTWorldCoordinate<'a>,
    )> {
        let mut coordinates = self.0.children().filter_map(CSTWorldCoordinate::cast);

        let x = coordinates.next()?;
        let y = coordinates.next()?;
        let z = coordinates.next()?;

        Some((x, y, z))
    }
}
