use crate::identifier;
use minecraft_command_types::resource_location::ResourceLocation;
use nonempty::NonEmpty;
use parser_rs::{FnParser, SemanticTokenKind, Stream, char};

pub fn parse_resource_location<'a>(input: &mut Stream<'a>) -> Option<ResourceLocation> {
    (|input: &mut Stream<'a>| {
        let is_tag = char('#').optional().parse(input)?.is_some();

        let namespace = (|input: &mut Stream<'a>| {
            let namespace = identifier("resource location namespace").parse(input)?;
            char(':').parse(input)?;
            Some(namespace)
        })
        .attempt()
        .parse(input)?
        .map(ToString::to_string);

        let paths = NonEmpty::from_vec(
            identifier("resource location path segment")
                .map(ToString::to_string)
                .separated_by_one(char('/'))
                .parse(input)?,
        )
        .unwrap();

        Some(ResourceLocation::new(is_tag, namespace, paths))
    })
    .syntax(SemanticTokenKind::Function)
    .label("resource location")
    .parse(input)
}
