pub mod data;
pub mod difficulty;
pub mod enchant;
pub mod execute;
pub mod function;
pub mod r#return;
pub mod scoreboard;
pub mod summon;
pub mod tellraw;

use crate::command::difficulty::parse_difficulty_command;
use crate::command::enchant::parse_enchant_command;
use crate::command::execute::parse_execute_command;
use crate::command::function::parse_function_command;
use crate::command::r#return::parse_return_command;
use crate::command::scoreboard::parse_scoreboard_command;
use crate::command::tellraw::parse_tellraw_command;
use crate::command::{data::parse_data_command, summon::parse_summon_command};
use kelp_core::high::command::HighCommand;
use parser_rs::{combinators::choice::choice, fn_parser::FnParser, stream::Stream};

fn split_2<A, B>(input: Option<(A, Option<B>)>) -> (Option<A>, Option<B>) {
    match input {
        Some((t, r_opt)) => (Some(t), r_opt),
        None => (None, None),
    }
}

fn split_3<A, B, C>(input: Option<(A, Option<B>, Option<C>)>) -> (Option<A>, Option<B>, Option<C>) {
    match input {
        Some((a, b, c)) => (Some(a), b, c),
        None => (None, None, None),
    }
}

pub fn parse_command(input: &mut Stream) -> Option<HighCommand> {
    choice((
        parse_data_command,
        parse_difficulty_command,
        parse_enchant_command,
        parse_execute_command,
        parse_function_command,
        parse_return_command,
        parse_scoreboard_command,
        parse_summon_command,
        parse_tellraw_command,
    ))
    .label("command")
    .parse(input)
}
