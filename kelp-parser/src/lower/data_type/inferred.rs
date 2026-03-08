use kelp_core::data_type::high::{HighDataType, HighDataTypeKind};

use crate::{cst::CSTInferredDataType, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_inferred_data_type(node: CSTInferredDataType) -> Option<HighDataType> {
    let span = span_of_cst_node(&node);

    Some(HighDataTypeKind::Inferred.with_span(span))
}
