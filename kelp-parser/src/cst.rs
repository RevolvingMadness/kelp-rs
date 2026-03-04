include!(concat!(env!("OUT_DIR"), "/cst.rs"));

impl CSTRoot {
    pub fn cast(node: SyntaxNode) -> Result<Self, SyntaxNode> {
        if Self::can_cast(node.kind()) {
            Ok(Self { syntax: node })
        } else {
            Err(node)
        }
    }

    #[inline]
    #[must_use]
    pub fn syntax(self) -> SyntaxNode {
        self.syntax
    }
}
