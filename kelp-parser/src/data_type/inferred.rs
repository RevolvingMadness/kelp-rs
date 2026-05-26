use kelp_core::parsed::data_type::ParsedDataType;

use crate::cst::CSTInferredDataType;

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_inferred_data_type(_node: CSTInferredDataType) -> Option<ParsedDataType> {
    Some(ParsedDataType::Inferred)
}
