use kelp_core::high::data_type::DataType;

use crate::cst::CSTInferredDataType;

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_inferred_data_type(_node: CSTInferredDataType) -> Option<DataType> {
    Some(DataType::Inferred)
}
