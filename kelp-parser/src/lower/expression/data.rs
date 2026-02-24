use crate::{
    cst_node,
    lower::data::{nbt_path::CSTNBTPath, target::CSTDataTarget},
    parser::Parser,
    syntax::SyntaxKind,
};

cst_node!(CSTDataExpression, SyntaxKind::DataExpression);

impl<'a> CSTDataExpression<'a> {
    pub fn try_parse(parser: &mut Parser) -> bool {
        let checkpoint = parser.checkpoint();

        if !CSTDataTarget::try_parse(parser) {
            return false;
        }

        parser.start_node_at(checkpoint, SyntaxKind::DataExpression);

        parser.expect_inline_whitespace();

        CSTNBTPath::try_parse(parser);

        parser.finish_node();

        true
    }
    pub fn data_target(&self) -> Option<CSTDataTarget<'a>> {
        self.children().find_map(CSTDataTarget::cast)
    }

    pub fn nbt_path(&self) -> Option<CSTNBTPath<'a>> {
        self.children().find_map(CSTNBTPath::cast)
    }
}
