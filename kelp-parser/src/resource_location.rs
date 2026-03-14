use minecraft_command_types::resource_location::ResourceLocation;
use nonempty::NonEmpty;

use crate::{
    cst::{
        CSTResourceLocation, CSTResourceLocationNamespace, CSTResourceLocationPath,
        CSTResourceLocationPathSegment,
    },
    parser::Parser,
    syntax::SyntaxKind,
};

fn expect_paths(parser: &mut Parser) {
    parser.start_node(SyntaxKind::ResourceLocationPath);

    parser.start_node(SyntaxKind::ResourceLocationPathSegment);
    parser.expect_identifier("Expected at least one resource location path segment");
    parser.finish_node();

    while parser.try_bump_char('/') {
        parser.start_node(SyntaxKind::ResourceLocationPathSegment);
        parser.expect_identifier("Expected resource location path segment after separator");
        parser.finish_node();
    }

    parser.finish_node();
}

#[must_use]
pub fn try_parse_resource_location(parser: &mut Parser) -> bool {
    parser.start_node(SyntaxKind::ResourceLocation);

    if parser.peek_char() == Some('#') {
        parser.add_token(SyntaxKind::ResourceLocationTag, 1);
    }

    if let Some(namespace) = parser.peek_identifier()
        && parser.peek_nth_char(namespace.len()) == Some(':')
    {
        parser.start_node(SyntaxKind::ResourceLocationNamespace);
        parser.bump_identifier(namespace);
        parser.finish_node();

        parser.bump_char();
    }

    expect_paths(parser);

    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_resource_location_namespace(node: CSTResourceLocationNamespace) -> Option<String> {
    Some(node.identifier_token()?.text().to_string())
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_resource_location_path_segment(
    node: CSTResourceLocationPathSegment,
) -> Option<String> {
    Some(node.identifier_token()?.text().to_string())
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_resource_location_path(node: CSTResourceLocationPath) -> Vec<String> {
    node.path_segments()
        .filter_map(lower_resource_location_path_segment)
        .collect()
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_resource_location(node: CSTResourceLocation) -> Option<ResourceLocation> {
    let is_tag = node.pound_token().is_some();

    let namespace = match node.namespace().map(lower_resource_location_namespace) {
        None => None,
        Some(None) => return None,
        Some(Some(value)) => Some(value),
    };

    let paths = lower_resource_location_path(node.path()?);
    let paths = NonEmpty::from_vec(paths)?;

    Some(ResourceLocation::new(is_tag, namespace, paths))
}
