use std::slice::Iter;

use kelp_core::span::Span;

use crate::{
    cstlib::{error::CSTError, node::CSTNode, token::CSTToken},
    syntax::SyntaxKind,
};

pub mod error;
pub mod node;
pub mod token;

#[derive(Debug, Clone)]
pub enum CSTNodeType {
    Node(CSTNode),
    Token(CSTToken),
    Error(CSTError),
}

impl CSTNodeType {
    #[must_use]
    pub const fn as_node(&self) -> Option<&CSTNode> {
        if let Self::Node(node) = self {
            Some(node)
        } else {
            None
        }
    }

    #[must_use]
    pub const fn as_token(&self) -> Option<&CSTToken> {
        if let Self::Token(token) = self {
            Some(token)
        } else {
            None
        }
    }

    #[must_use]
    pub const fn as_error(&self) -> Option<&CSTError> {
        if let Self::Error(error) = self {
            Some(error)
        } else {
            None
        }
    }

    #[must_use]
    pub const fn kind(&self) -> Option<SyntaxKind> {
        Some(match self {
            Self::Node(node) => node.kind,
            Self::Token(token) => token.kind,
            Self::Error { .. } => return None,
        })
    }

    #[must_use]
    pub const fn span(&self) -> Span {
        match self {
            Self::Node(node) => node.span,
            Self::Token(token) => token.span,
            Self::Error(error) => error.span,
        }
    }

    #[must_use]
    pub fn is_kind(&self, kind: SyntaxKind) -> bool {
        self.kind() == Some(kind)
    }

    pub fn children(&self) -> Iter<'_, Self> {
        match self {
            Self::Node(node) => node.children.iter(),
            _ => [].iter(),
        }
    }

    pub fn children_tokens(&self) -> impl Iterator<Item = &CSTToken> {
        if let Self::Node(node) = self {
            #[allow(clippy::iter_on_single_items)]
            Some(node.children.iter().filter_map(Self::as_token))
                .into_iter()
                .flatten()
        } else {
            #[allow(clippy::iter_on_empty_collections)]
            None.into_iter().flatten()
        }
    }

    pub fn print(&self, text: &str, depth: usize) {
        let indent = "  ".repeat(depth);
        match self {
            Self::Node(node) => {
                println!("{}{:?}@{}", indent, node.kind, node.span);
                for child in &node.children {
                    child.print(text, depth + 1);
                }
            }
            Self::Token(token) => {
                println!(
                    "{}{:?}@{} ({})",
                    indent,
                    token.kind,
                    token.span,
                    token.text(text).escape_default(),
                );
            }
            Self::Error(error) => {
                println!("{}Error@{}({})", indent, error.span, error.message);
            }
        }
    }
}
