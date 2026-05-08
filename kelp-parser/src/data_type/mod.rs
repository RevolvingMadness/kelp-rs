use kelp_core::high::data_type::DataType;

use crate::{
    cst::{CSTDataType, CSTTypedCompoundDataTypeField},
    data_type::{
        inferred::lower_inferred_data_type,
        path::{lower_path_data_type, try_parse_path_data_type},
        reference::{lower_reference_data_type, try_parse_reference_data_type},
        typed_compound::try_parse_typed_compound_data_type,
    },
    parser::Parser,
    syntax::SyntaxKind,
};

pub mod generics;
pub mod inferred;
pub mod path;
pub mod reference;
pub mod typed_compound;

#[must_use]
pub fn try_parse_data_type(parser: &mut Parser) -> bool {
    match parser.peek_char() {
        Some('&') => try_parse_reference_data_type(parser),
        Some('(') => try_parse_tuple_or_unit_data_type(parser),
        Some('!') => {
            parser.start_node(SyntaxKind::NeverDataType);
            parser.bump_char();
            parser.finish_node();

            true
        }
        Some('{') => try_parse_typed_compound_data_type(parser),
        _ => parser.peek_identifier().is_some_and(|identifier| {
            if identifier == "_" {
                parser.start_node(SyntaxKind::InferredDataType);
                parser.bump_char();
                parser.finish_node();

                true
            } else {
                try_parse_path_data_type(parser)
            }
        }),
    }
}

#[must_use]
fn try_parse_tuple_or_unit_data_type(parser: &mut Parser) -> bool {
    let checkpoint = parser.checkpoint();
    parser.bump_char();
    parser.skip_whitespace();

    if parser.peek_char() == Some(')') {
        parser.start_node_at(checkpoint, SyntaxKind::UnitDataType);
        parser.bump_char();
        parser.finish_node();
        return true;
    }

    let _ = try_parse_data_type(parser);
    parser.skip_whitespace();

    while parser.try_bump_char(',') {
        parser.skip_whitespace();

        if parser.peek_char() == Some(')') {
            break;
        }

        let _ = try_parse_data_type(parser);
        parser.skip_whitespace();
    }

    parser.start_node_at(checkpoint, SyntaxKind::TupleDataType);
    parser.expect_char(')', "Expected ')'");
    parser.finish_node();

    true
}

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_typed_compound_data_type_field(
    node: CSTTypedCompoundDataTypeField,
) -> Option<(String, DataType)> {
    let name_token = node.name()?;
    let name = name_token.text();
    let data_type = lower_data_type(node.data_type()?)?;

    Some((name.to_owned(), data_type))
}

#[must_use]
pub fn lower_data_type(node: CSTDataType) -> Option<DataType> {
    match node {
        CSTDataType::ReferenceDataType(node) => lower_reference_data_type(node),
        CSTDataType::TupleDataType(node) => {
            let data_types = node.data_types().filter_map(lower_data_type).collect();

            Some(DataType::Tuple(data_types))
        }
        CSTDataType::UnitDataType(..) => Some(DataType::Unit),
        CSTDataType::NeverDataType(..) => Some(DataType::Never),
        CSTDataType::TypedCompoundDataType(data_type) => {
            let fields = data_type
                .fields()
                .filter_map(lower_typed_compound_data_type_field)
                .collect();

            Some(DataType::TypedCompound(fields))
        }
        CSTDataType::PathDataType(node) => lower_path_data_type(node),
        CSTDataType::InferredDataType(node) => lower_inferred_data_type(node),
    }
}
