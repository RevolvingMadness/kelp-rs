use std::collections::HashSet;

use crate::{
    high::{
        environment::{r#type::HighGenericId, value::function::HighFunctionId},
        semantic_analysis::RegularFunctionModifiers,
    },
    low::{
        data_type::unresolved::UnresolvedDataType, expression::unresolved::UnresolvedExpression,
        pattern::UnresolvedPattern,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighRegularFunctionId(pub u32);

#[derive(Debug, Clone)]
pub struct HighRegularFunctionDeclaration {
    pub name: String,
    pub modifiers: RegularFunctionModifiers,
    pub generic_ids: Vec<HighGenericId>,
    pub parameters: Vec<(Option<UnresolvedPattern>, UnresolvedDataType)>,
    pub return_type: UnresolvedDataType,
    pub body: Option<Box<UnresolvedExpression>>,
    pub calls: Option<HashSet<HighFunctionId>>,
}
