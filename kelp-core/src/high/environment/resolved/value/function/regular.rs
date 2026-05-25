use std::collections::HashSet;

use la_arena::Idx;

use crate::{
    high::{
        environment::resolved::{r#type::HighGenericId, value::function::HighFunctionId},
        semantic_analysis::RegularFunctionModifiers,
    },
    low::{
        data_type::unresolved::UnresolvedDataType, expression::unresolved::UnresolvedExpression,
        pattern::UnresolvedPattern,
    },
    span::Span,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighRegularFunctionId(pub u32);

#[derive(Debug, Clone)]
pub struct ResolvedRegularFunctionDeclaration {
    pub name: String,
    pub modifiers: RegularFunctionModifiers,
    pub generic_ids: Vec<HighGenericId>,
    pub parameters: Vec<(Option<Idx<UnresolvedPattern>>, UnresolvedDataType)>,
    pub return_type: UnresolvedDataType,
    pub body: Option<Idx<UnresolvedExpression>>,
    pub calls: HashSet<(Span, HighFunctionId)>,
}
