use kelp_core::span::Span;

use crate::{
    cstlib::{error::CSTError, node::CSTNode, token::CSTToken},
    syntax::SyntaxKind,
};

pub mod error;
pub mod node;
pub mod token;

#[derive(Debug, Clone)]
pub enum CSTNodeType<'a> {
    Node(CSTNode<'a>),
    Token(CSTToken<'a>),
    Error(CSTError),
}

impl<'a> CSTNodeType<'a> {
    #[must_use]
    pub fn as_node(&'a self) -> Option<&'a CSTNode<'a>> {
        if let CSTNodeType::Node(node) = self {
            Some(node)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_token(&'a self) -> Option<&'a CSTToken<'a>> {
        if let CSTNodeType::Token(token) = self {
            Some(token)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_error(&self) -> Option<&CSTError> {
        if let CSTNodeType::Error(error) = self {
            Some(error)
        } else {
            None
        }
    }

    #[must_use]
    pub fn kind(&self) -> Option<SyntaxKind> {
        Some(match self {
            CSTNodeType::Node(node) => node.kind,
            CSTNodeType::Token(token) => token.kind,
            CSTNodeType::Error { .. } => return None,
        })
    }

    #[must_use]
    pub fn span(&self) -> Span {
        match self {
            CSTNodeType::Node(node) => node.span,
            CSTNodeType::Token(token) => token.span,
            CSTNodeType::Error(error) => error.span,
        }
    }

    #[must_use]
    pub fn is_kind(&self, kind: SyntaxKind) -> bool {
        self.kind() == Some(kind)
    }

    pub fn children(&'a self) -> impl DoubleEndedIterator<Item = &'a CSTNodeType<'a>> {
        match self {
            CSTNodeType::Node(node) => node.children.iter(),
            _ => [].iter(),
        }
    }

    pub fn children_tokens(&'a self) -> impl Iterator<Item = &'a CSTToken<'a>> {
        if let CSTNodeType::Node(node) = self {
            Some(node.children.iter().filter_map(CSTNodeType::as_token))
                .into_iter()
                .flatten()
        } else {
            None.into_iter().flatten()
        }
    }

    #[must_use]
    pub fn text(&self) -> &str {
        if let CSTNodeType::Token(token) = self {
            token.text
        } else {
            ""
        }
    }

    pub fn print(&self, depth: usize) {
        let indent = "  ".repeat(depth);
        match self {
            CSTNodeType::Node(node) => {
                println!("{}{:?}@{}", indent, node.kind, node.span);
                for child in &node.children {
                    child.print(depth + 1);
                }
            }
            CSTNodeType::Token(token) => {
                println!(
                    "{}{:?}@{} ({})",
                    indent,
                    token.kind,
                    token.span,
                    token.text.escape_default()
                );
            }
            CSTNodeType::Error(error) => {
                println!("{}Error@{}({})", indent, error.span, error.message);
            }
        }
    }
}
