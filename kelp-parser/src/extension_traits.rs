use kelp_core::span::Span;
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange, ast::AstNode};

use crate::{lower_context::LowerContext, parser::Parser};

#[inline]
#[must_use]
fn text_range_to_span(range: TextRange) -> Span {
    Span {
        start: range.start().into(),
        end: range.end().into(),
    }
}

pub trait AstNodeExt: AstNode + Sized {
    fn span(&self) -> Span;
}

impl<T: AstNode> AstNodeExt for T {
    fn span(&self) -> Span {
        self.syntax().span()
    }
}

pub trait SyntaxNodeExt {
    fn span(&self) -> Span;
}

impl<T: Language> SyntaxNodeExt for SyntaxNode<T> {
    fn span(&self) -> Span {
        text_range_to_span(self.text_range())
    }
}

pub trait SyntaxTokenExt {
    fn span(&self) -> Span;
}

impl<T: Language> SyntaxTokenExt for SyntaxToken<T> {
    fn span(&self) -> Span {
        text_range_to_span(self.text_range())
    }
}

pub trait ParsableAstNode: AstNodeExt {
    fn try_parse(parser: &mut Parser) -> bool;

    fn expect(parser: &mut Parser, message: &str) -> bool {
        if Self::try_parse(parser) {
            return true;
        }

        parser.error(message);

        false
    }
}

pub trait RecoverableAstNode: ParsableAstNode {
    fn recover(parser: &mut Parser);

    fn recover_expect(parser: &mut Parser, message: &str) -> bool {
        if Self::try_parse(parser) {
            return true;
        }

        parser.error(message);

        Self::recover(parser);

        false
    }
}

pub trait LowerableAstNode: AstNodeExt {
    type Lowered;

    #[must_use]
    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered>;
}
