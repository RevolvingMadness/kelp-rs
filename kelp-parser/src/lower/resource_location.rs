use minecraft_command_types::resource_location::ResourceLocation;
use nonempty::NonEmpty;

use crate::{
    cst_node,
    parser::Parser,
    semantic_token::{SemanticToken, SemanticTokenType},
    syntax::SyntaxKind,
};

cst_node!(CSTResourceLocationPaths, SyntaxKind::ResourceLocationPaths);

impl CSTResourceLocationPaths<'_> {
    #[must_use]
    pub fn paths<'b>(&self, text: &'b str) -> Vec<&'b str> {
        self.0
            .children_tokens()
            .filter_map(|token| {
                if token.kind == SyntaxKind::Identifier {
                    Some(token.text(text))
                } else {
                    None
                }
            })
            .collect()
    }
}

cst_node!(CSTResourceLocation, SyntaxKind::ResourceLocation);

impl CSTResourceLocation<'_> {
    fn try_parse_paths(parser: &mut Parser) -> bool {
        let mut result = true;

        parser.start_node(SyntaxKind::ResourceLocationPaths);

        if !parser.expect_identifier("Expected at least one resource location path segment") {
            result = false;
        }

        while parser.try_bump_char('/') {
            parser.expect_identifier("Expected resource location path segment after separator");
        }

        parser.finish_node();

        result
    }

    #[must_use]
    pub fn try_parse(parser: &mut Parser) -> bool {
        parser.start_node(SyntaxKind::ResourceLocation);

        if parser.peek_char() == Some('#') {
            parser.start_bump_finish_node(SyntaxKind::ResourceLocationTag, 1);
        }

        if let Some(namespace) = parser.peek_identifier()
            && parser.peek_nth_char(namespace.len()) == Some(':')
        {
            parser.start_node(SyntaxKind::ResourceLocationNamespace);
            parser.bump_identifier(namespace);
            parser.finish_node();

            parser.bump_char();
        }

        if !CSTResourceLocation::try_parse_paths(parser) {
            parser.finish_node();

            return false;
        }

        parser.finish_node();

        true
    }

    #[must_use]
    pub fn lower(self, text: &str) -> Option<ResourceLocation> {
        let is_tag = self.is_tag();

        let namespace = self.namespace(text).map(ToString::to_string);

        let paths = self
            .paths(text)
            .into_iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>();

        Some(ResourceLocation::new(
            is_tag,
            namespace,
            NonEmpty::from_vec(paths).unwrap(),
        ))
    }

    #[must_use]
    pub fn is_tag(&self) -> bool {
        self.0
            .children_tokens()
            .any(|token| token.kind == SyntaxKind::ResourceLocationTag)
    }

    #[must_use]
    pub fn namespace<'b>(&self, text: &'b str) -> Option<&'b str> {
        self.children().find_map(|node| {
            if node.kind()? == SyntaxKind::ResourceLocationNamespace {
                Some(
                    (node.children_tokens().find_map(|token| {
                        if token.kind == SyntaxKind::Identifier {
                            Some(token.text(text))
                        } else {
                            None
                        }
                    }))
                    .unwrap(),
                )
            } else {
                None
            }
        })
    }

    #[must_use]
    pub fn paths<'b>(&self, text: &'b str) -> Vec<&'b str> {
        self.0
            .children()
            .find_map(|node| {
                if node.kind()? == SyntaxKind::ResourceLocationPaths {
                    Some(
                        node.children_tokens()
                            .filter_map(|token| {
                                if token.kind == SyntaxKind::Identifier {
                                    Some(token.text(text))
                                } else {
                                    None
                                }
                            })
                            .collect(),
                    )
                } else {
                    None
                }
            })
            .unwrap()
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        tokens.push(SemanticToken::new(self.span(), SemanticTokenType::Function));
    }
}
