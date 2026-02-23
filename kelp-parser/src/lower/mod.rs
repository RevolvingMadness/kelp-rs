use kelp_core::statement::Statement;

use crate::{cstlib::CSTNodeType, lower::statement::CSTStatement};

pub mod coordinates;
pub mod cst;
pub mod data;
pub mod data_type;
pub mod entity_selector;
pub mod expression;
pub mod pattern;
pub mod resource_location;
pub mod statement;

pub struct Lowerer;

impl Lowerer {
    pub fn lower_root(node: &CSTNodeType) -> Vec<Statement> {
        node.children()
            .filter_map(CSTStatement::cast)
            .filter_map(CSTStatement::lower)
            .collect()
    }
}
