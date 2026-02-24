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
    pub fn as_node(&self) -> Option<&CSTNode> {
        if let CSTNodeType::Node(node) = self {
            Some(node)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_token(&self) -> Option<&CSTToken> {
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

    pub fn children(&self) -> impl DoubleEndedIterator<Item = &CSTNodeType> {
        match self {
            CSTNodeType::Node(node) => node.children.iter(),
            _ => [].iter(),
        }
    }

    pub fn children_tokens(&self) -> impl Iterator<Item = &CSTToken> {
        if let CSTNodeType::Node(node) = self {
            Some(node.children.iter().filter_map(CSTNodeType::as_token))
                .into_iter()
                .flatten()
        } else {
            None.into_iter().flatten()
        }
    }

    pub fn print(&self, text: &str, depth: usize) {
        let indent = "  ".repeat(depth);
        match self {
            CSTNodeType::Node(node) => {
                println!("{}{:?}@{}", indent, node.kind, node.span);
                for child in &node.children {
                    child.print(text, depth + 1);
                }
            }
            CSTNodeType::Token(token) => {
                println!(
                    "{}{:?}@{} ({})",
                    indent,
                    token.kind,
                    token.span,
                    token.text(text).escape_default(),
                );
            }
            CSTNodeType::Error(error) => {
                println!("{}Error@{}({})", indent, error.span, error.message);
            }
        }
    }
}
