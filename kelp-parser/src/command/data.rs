use crate::command::HighCommand;
use crate::command::{split_2, split_3};
use crate::coordinate::parse_coordinates;
use crate::entity_selector::parse_entity_selector;
use crate::expression::{expression, parse_compound};
use crate::resource_location::parse_resource_location;
use crate::{float, integer, required_inline_whitespace, string, whitespace};
use kelp_core::high::command::data::{HighDataCommand, HighDataCommandModification};
use kelp_core::high::data::{HighDataTarget, HighDataTargetKind};
use kelp_core::high::nbt_path::{HighNbtPath, HighNbtPathNode};
use kelp_core::high::snbt_string::HighSNBTString;
use minecraft_command_types::command::data::DataCommandModificationMode;
use minecraft_command_types::snbt::SNBTString;
use nonempty::nonempty;
use parser_rs::{
    combinators::{char, choice::choice, literal, suggest_literal},
    fn_parser::FnParser,
    semantic_token::SemanticTokenKind,
    stream::Stream,
};

pub fn parse_data_target<'a>(prefix_optional: bool) -> impl FnParser<'a, Output = HighDataTarget> {
    move |input: &mut Stream<'a>| {
        // TODO maybe a better way to do this?
        if prefix_optional {
            choice((
                |input: &mut Stream<'a>| {
                    suggest_literal("block")
                        .syntax(SemanticTokenKind::Class)
                        .parse(input)?;
                    required_inline_whitespace(input)?;

                    let coordinates = parse_coordinates(input)?;

                    Some(HighDataTargetKind::Block(coordinates))
                },
                |input: &mut Stream<'a>| {
                    suggest_literal("storage")
                        .syntax(SemanticTokenKind::Class)
                        .parse(input)?;
                    required_inline_whitespace(input)?;

                    let location = parse_resource_location(input)?;

                    Some(HighDataTargetKind::Storage(location))
                },
                |input: &mut Stream<'a>| {
                    suggest_literal("entity")
                        .syntax(SemanticTokenKind::Class)
                        .parse(input)?;
                    required_inline_whitespace(input)?;

                    let selector = parse_entity_selector(input)?;

                    Some(HighDataTargetKind::Entity(selector))
                },
                parse_coordinates.map(HighDataTargetKind::Block),
                parse_resource_location.map(HighDataTargetKind::Storage),
                parse_entity_selector.map(HighDataTargetKind::Entity),
            ))
            .parse(input)
        } else {
            choice((
                |input: &mut Stream<'a>| {
                    literal("block")
                        .syntax(SemanticTokenKind::Class)
                        .parse(input)?;
                    required_inline_whitespace(input)?;

                    let coordinates = parse_coordinates(input)?;

                    Some(HighDataTargetKind::Block(coordinates))
                },
                |input: &mut Stream<'a>| {
                    literal("storage")
                        .syntax(SemanticTokenKind::Class)
                        .parse(input)?;
                    required_inline_whitespace(input)?;

                    let location = parse_resource_location(input)?;

                    Some(HighDataTargetKind::Storage(location))
                },
                |input: &mut Stream<'a>| {
                    literal("entity")
                        .syntax(SemanticTokenKind::Class)
                        .parse(input)?;
                    required_inline_whitespace(input)?;

                    let selector = parse_entity_selector(input)?;

                    Some(HighDataTargetKind::Entity(selector))
                },
            ))
            .parse(input)
        }
        .map(|kind| HighDataTarget {
            is_generated: false,
            kind,
        })
    }
}

pub fn parse_nbt_path_root(input: &mut Stream) -> Option<HighNbtPathNode> {
    parse_compound
        .parse(input)
        .map(HighNbtPathNode::RootCompound)
}

pub fn parse_nbt_path_named(input: &mut Stream) -> Option<HighNbtPathNode> {
    let (name_span, name) = string.spanned().parse(input)?;

    let compound = parse_compound.optional().parse(input)?;

    Some(HighNbtPathNode::Named(
        HighSNBTString {
            span: name_span,
            snbt_string: SNBTString(false, name),
        },
        compound,
    ))
}

pub fn parse_nbt_path_index(input: &mut Stream) -> Option<HighNbtPathNode> {
    char('[').parse(input)?;
    whitespace(input)?;

    let value = expression.map(Box::new).optional().parse(input)?;

    char(']').parse(input)?;

    Some(HighNbtPathNode::Index(value))
}

pub fn parse_nbt_path(input: &mut Stream) -> Option<HighNbtPath> {
    (|input: &mut Stream| {
        let start = choice((
            parse_nbt_path_named,
            parse_nbt_path_root,
            parse_nbt_path_index,
        ))
        .parse(input)?;

        let mut nodes = nonempty![start];

        let rest = (|input: &mut Stream| {
            let has_dot = char('.').optional().parse(input)?.is_some();

            if has_dot {
                choice((parse_nbt_path_named, parse_nbt_path_index)).parse(input)
            } else {
                parse_nbt_path_index(input)
            }
        })
        .many::<Vec<_>>()
        .parse(input)?;

        nodes.extend(rest);

        Some(HighNbtPath(nodes))
    })
    .label("nbt path")
    .parse(input)
}

pub fn parse_data_command(input: &mut Stream) -> Option<HighCommand> {
    const TARGET_PARAMETER: (&str, Option<&str>) = ("<target>", Some("What to operate on"));
    const PATH_PARAMETER: (&str, Option<&str>) =
        ("<path>", Some("The path in <target> to operate on"));

    let data_command = (|input: &mut Stream| {
        suggest_literal("data").syntax_keyword().parse(input)?;
        required_inline_whitespace(input)?;
        choice((
            (|input: &mut Stream| {
                suggest_literal("get").parse(input)?;
                required_inline_whitespace(input)?;
                let target = parse_data_target(true).next_signature_parameter().parse(input)?;
                let (path, scale) = split_2(
                    (|input: &mut Stream| {
                        required_inline_whitespace(input)?;
                        let path = parse_nbt_path.next_signature_parameter().parse(input)?;

                        let scale = (|input: &mut Stream| {
                            required_inline_whitespace(input)?;
                            float.next_signature_parameter().parse(input)
                        })
                        .optional()
                        .parse(input)?;

                        Some((path, scale))
                    })
                    .optional()
                    .parse(input)?,
                );

                Some(HighDataCommand::Get(target, path, scale))
            })
            .signature(0),
            (|input: &mut Stream| {
                suggest_literal("merge").parse(input)?;
                required_inline_whitespace(input)?;
                let target = parse_data_target(true).next_signature_parameter().parse(input)?;
                required_inline_whitespace(input)?;
                let expression = expression.next_signature_parameter().parse(input)?;

                Some(HighDataCommand::Merge(target, Box::new(expression)))
            })
            .signature(1),
            (|input: &mut Stream| {
                suggest_literal("modify").parse(input)?;
                required_inline_whitespace(input)?;
                let target = parse_data_target(true).next_signature_parameter().parse(input)?;
                required_inline_whitespace(input)?;
                let path = parse_nbt_path.next_signature_parameter().parse(input)?;
                required_inline_whitespace(input)?;
                let mode = choice((
                    suggest_literal("append")
                        .signature(0)
                        .map_to(DataCommandModificationMode::Append),
                    (|input: &mut Stream| {
                        suggest_literal("insert").parse(input)?;
                        required_inline_whitespace(input)?;
                        let index = integer.next_signature_parameter().parse(input)?;

                        Some(DataCommandModificationMode::Insert(index))
                    })
                    .signature(1),
                    suggest_literal("merge")
                        .signature(2)
                        .map_to(DataCommandModificationMode::Merge),
                    suggest_literal("prepend")
                        .signature(3)
                        .map_to(DataCommandModificationMode::Prepend),
                    suggest_literal("set")
                        .signature(4)
                        .map_to(DataCommandModificationMode::Set),
                ))
                .signatures(&[
                    ("append", &[], Some("Appends <source> onto <target>")),
                    (
                        "insert <index>",
                        &[("<index>", Some("The index to insert into"))],
                        Some("Inserts <source> into <target> at <index>"),
                    ),
                    ("merge", &[], Some("Merges <source> into <target>")),
                    (
                        "prepend",
                        &[],
                        Some("Prepends <source> in front of <target>"),
                    ),
                    ("set", &[], Some("Sets <source> to <target>")),
                ])
                .next_signature_parameter()
                .parse(input)?;
                required_inline_whitespace(input)?;
                let modification = (|input: &mut Stream| {
                    choice((
                        (|input: &mut Stream| {
                            suggest_literal("from").parse(input)?;
                            required_inline_whitespace(input)?;
                            let target =
                                parse_data_target(true).next_signature_parameter().parse(input)?;
                            let path = (|input: &mut Stream| {
                                required_inline_whitespace(input)?;
                                parse_nbt_path.next_signature_parameter().parse(input)
                            })
                            .optional()
                            .parse(input)?;

                            Some(HighDataCommandModification::From(target, path))
                        })
                        .signature(0),
                        (|input: &mut Stream| {
                            suggest_literal("string").parse(input)?;
                            required_inline_whitespace(input)?;
                            let target =
                                parse_data_target(true).next_signature_parameter().parse(input)?;
                            let (path, start, end) = split_3(
                                (|input: &mut Stream| {
                                    required_inline_whitespace(input)?;
                                    let path =
                                        parse_nbt_path.next_signature_parameter().parse(input)?;
                                    let (start, end) = split_2(
                                        (|input: &mut Stream| {
                                            required_inline_whitespace(input)?;
                                            let start = integer
                                                .next_signature_parameter()
                                                .parse(input)?;

                                            let end = (|input: &mut Stream| {
                                                required_inline_whitespace(input)?;
                                                integer
                                                    .next_signature_parameter()
                                                    .parse(input)
                                            })
                                            .optional()
                                            .parse(input)?;

                                            Some((start, end))
                                        })
                                        .optional()
                                        .parse(input)?,
                                    );

                                    Some((path, start, end))
                                })
                                .optional()
                                .parse(input)?,
                            );

                            Some(HighDataCommandModification::String(target, path, start, end))
                        })
                        .signature(1),
                        (|input: &mut Stream| {
                            suggest_literal("value").parse(input)?;
                            required_inline_whitespace(input)?;
                            let value = expression.next_signature_parameter().parse(input)?;

                            Some(HighDataCommandModification::Value(Box::new(value)))
                        })
                        .signature(2),
                    ))
                    .signatures(&[
                        (
                            "from <source> [<sourcePath>]",
                            &[("<source>", Some("Where to get the value from")), ("[<sourcePath>]", Some("The path of nbt"))],
                            Some("Copies the value from <source> (optionally at [<sourcePath>]) into <target> at <path> ")
                        ),
                        (
                            "string <source> [<sourcePath>] [<start>] [<end>]",
                            &[("<source>", Some("Where to slice the string from")), ("[<sourcePath>]", Some("The path of nbt")), ("[<start>]", Some("The start index")), ("[<end>]", Some("The end index"))],
                            None
                        ),
                        ("value <snbt>", &[("<snbt>", Some("The value to set to"))], Some("Sets the value at <path> in <target> to <snbt>")),
                    ])
                    .parse(input)
                })
                .next_signature_parameter()
                .parse(input)?;

                Some(HighDataCommand::Modify(target, path, mode, Box::new(modification)))
            })
            .signature(2),
            (|input: &mut Stream| {
                suggest_literal("remove").parse(input)?;
                required_inline_whitespace(input)?;
                let target = parse_data_target(true).next_signature_parameter().parse(input)?;
                required_inline_whitespace(input)?;
                let path = parse_nbt_path.next_signature_parameter().parse(input)?;

                Some(HighDataCommand::Remove(target, path))
            })
            .signature(3),
        ))
        .parse(input)
    })
    .signatures(&[
        (
            "data get <target> <path> [<scale>]",
            &[
                TARGET_PARAMETER,
                PATH_PARAMETER,
                ("[<scale>]", Some("The amount to scale the value at <path>")),
            ],
            Some("Gets the data at <path> from <target>"),
        ),
        (
            "data merge <target> <nbt>",
            &[TARGET_PARAMETER, ("<nbt>", Some("The snbt data to merge"))],
            Some("merges <nbt> into <target>"),
        ),
        (
            "data modify <target> <path> <mode> <source>",
            &[
                TARGET_PARAMETER,
                PATH_PARAMETER,
                ("<mode>", Some("How to modify <target> at <path>")),
                ("<source>", Some("The value to modify <target> at <path>")),
            ],
            Some("Modifies <path> in <target> to be <source> with the operation of <mode>")
        ),
        (
            "data remove <target> <path>",
            &[TARGET_PARAMETER, PATH_PARAMETER],
            Some("Removes the data in <target> at <path>")
        ),
    ])
    .parse(input)?;

    Some(HighCommand::Data(data_command))
}
