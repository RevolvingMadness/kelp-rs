#[macro_export]
macro_rules! cst_node {
    ($name:ident, $kind:pat) => {
        #[derive(Debug, Clone, Copy)]
        pub struct $name<'a>(pub &'a $crate::cstlib::CSTNodeType);

        impl<'a> $name<'a> {
            pub const fn cast(node: &'a $crate::cstlib::CSTNodeType) -> Option<Self> {
                if matches!(node.kind(), Some($kind)) {
                    Some(Self(node))
                } else {
                    None
                }
            }

            pub const fn span(&self) -> kelp_core::span::Span {
                self.0.span()
            }

            pub fn children(&self) -> std::slice::Iter<'a, $crate::cstlib::CSTNodeType> {
                self.0.children()
            }
        }
    };
}
