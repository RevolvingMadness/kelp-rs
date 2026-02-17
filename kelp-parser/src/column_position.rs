use crate::coordinate::parse_world_coordinate;
use crate::required_inline_whitespace;
use minecraft_command_types::column_position::ColumnPosition;
use parser_rs::{fn_parser::FnParser, stream::Stream};

pub fn parse_column_position(input: &mut Stream) -> Option<ColumnPosition> {
    let x = parse_world_coordinate(false).parse(input)?;

    required_inline_whitespace(input)?;

    let z = parse_world_coordinate(true).parse(input)?;

    Some(ColumnPosition { x, z })
}
