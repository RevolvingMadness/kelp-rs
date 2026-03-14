use kelp_core::data_type::high::{DataTypeKind, DataType};

use crate::{cst::CSTInferredDataType, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_inferred_data_type(node: CSTInferredDataType) -> Option<DataType> {
    let span = span_of_cst_node(&node);

    Some(DataTypeKind::Inferred.with_span(span))
}
