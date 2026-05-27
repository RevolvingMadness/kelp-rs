use kelp_core::parsed::data_type::ParsedDataType;

use crate::{
    cst::CSTInferredDataType, extension_traits::LowerableAstNode, lower_context::LowerContext,
};

impl LowerableAstNode for CSTInferredDataType {
    type Lowered = ParsedDataType;

    fn lower(&self, _ctx: &mut LowerContext) -> Option<Self::Lowered> {
        Some(ParsedDataType::Inferred)
    }
}
