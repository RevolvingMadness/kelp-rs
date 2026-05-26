use std::collections::HashSet;

use la_arena::Idx;

use crate::{
    parsed::{
        environment::resolved::{r#type::HighGenericId, value::function::HighFunctionId},
        semantic_analysis::RegularFunctionModifiers,
    },
    span::Span,
    typed::{
        data_type::unresolved::UnresolvedDataType, expression::typed::TypedExpressionId,
        pattern::TypedPattern,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighRegularFunctionId(pub u32);

#[derive(Debug, Clone)]
pub struct ResolvedRegularFunctionDeclaration {
    pub name: String,
    pub modifiers: RegularFunctionModifiers,
    pub generic_ids: Vec<HighGenericId>,
    pub parameters: Vec<(Option<Idx<TypedPattern>>, UnresolvedDataType)>,
    pub return_type: UnresolvedDataType,
    pub body: Option<TypedExpressionId>,
    pub calls: HashSet<(Span, HighFunctionId)>,
}
