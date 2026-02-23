use ordered_float::NotNan;

use crate::{cst_node, syntax::SyntaxKind};

cst_node!(CSTLocalCoordinate, SyntaxKind::LocalCoordinate);

impl<'a> CSTLocalCoordinate<'a> {
    fn lower(&self) -> Option<NotNan<f32>> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::FractionalValue {
                Some(token.text.parse().unwrap())
            } else {
                None
            }
        })
    }
}

cst_node!(CSTLocalCoordinates, SyntaxKind::LocalCoordinates);

impl<'a> CSTLocalCoordinates<'a> {
    #[allow(clippy::type_complexity)]
    pub(crate) fn coordinates(
        self,
    ) -> (
        Option<NotNan<f32>>,
        Option<NotNan<f32>>,
        Option<NotNan<f32>>,
    ) {
        let mut coordinates = self
            .0
            .children()
            .filter_map(|node| CSTLocalCoordinate::cast(node)?.lower());

        let x = coordinates.next();
        let y = coordinates.next();
        let z = coordinates.next();

        (x, y, z)
    }
}
