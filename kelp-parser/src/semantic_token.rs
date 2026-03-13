use kelp_core::span::Span;
use rowan::NodeOrToken;

use crate::{
    span::text_range_to_span,
    syntax::{SyntaxKind, SyntaxNode},
};

#[derive(Debug)]
pub struct SemanticToken {
    pub span: Span,
    pub type_: SemanticTokenType,
    pub modifiers: Vec<SemanticTokenModifier>,
}

impl SemanticToken {
    #[must_use]
    pub const fn new(span: Span, type_: SemanticTokenType) -> Self {
        Self {
            span,
            type_,
            modifiers: Vec::new(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum SemanticTokenType {
    Namespace,
    Type,
    Class,
    Enum,
    Interface,
    Struct,
    TypeParameter,
    Parameter,
    Variable,
    Property,
    EnumMember,
    Event,
    Function,
    Method,
    Macro,
    Keyword,
    Modifier,
    Comment,
    String,
    Number,
    Regexp,
    Operator,
    Decorator,
}

impl SemanticTokenType {
    #[must_use]
    pub const fn to_index(self) -> u32 {
        match self {
            Self::Namespace => 0,
            Self::Type => 1,
            Self::Class => 2,
            Self::Enum => 3,
            Self::Interface => 4,
            Self::Struct => 5,
            Self::TypeParameter => 6,
            Self::Parameter => 7,
            Self::Variable => 8,
            Self::Property => 9,
            Self::EnumMember => 10,
            Self::Event => 11,
            Self::Function => 12,
            Self::Method => 13,
            Self::Macro => 14,
            Self::Keyword => 15,
            Self::Modifier => 16,
            Self::Comment => 17,
            Self::String => 18,
            Self::Number => 19,
            Self::Regexp => 20,
            Self::Operator => 21,
            Self::Decorator => 22,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum SemanticTokenModifier {}

impl SemanticTokenModifier {
    #[must_use]
    pub const fn to_bit(self) -> u32 {
        match self {}
    }
}

#[must_use]
pub fn collect_semantic_tokens(node: &SyntaxNode) -> Vec<SemanticToken> {
    let mut tokens = Vec::new();

    for node_or_token in node.descendants_with_tokens() {
        match node_or_token {
            NodeOrToken::Node(node) => {
                let semantic_token_type = match node.kind() {
                    SyntaxKind::ResourceLocation => Some(SemanticTokenType::Function),
                    SyntaxKind::VariableExpression => Some(SemanticTokenType::Variable),
                    _ => None,
                };

                if let Some(semantic_token_type) = semantic_token_type {
                    tokens.push(SemanticToken::new(
                        text_range_to_span(node.text_range()),
                        semantic_token_type,
                    ));
                }
            }

            NodeOrToken::Token(token) => {
                let semantic_token_type = match token.kind() {
                    SyntaxKind::ScoreKeyword
                    | SyntaxKind::EntityKeyword
                    | SyntaxKind::BlockKeyword
                    | SyntaxKind::StorageKeyword
                    | SyntaxKind::TellrawKeyword
                    | SyntaxKind::FunctionKeyword
                    | SyntaxKind::IfKeyword
                    | SyntaxKind::WhileKeyword
                    | SyntaxKind::LoopKeyword
                    | SyntaxKind::TypeKeyword
                    | SyntaxKind::ElseKeyword
                    | SyntaxKind::LetKeyword
                    | SyntaxKind::ToKeyword
                    | SyntaxKind::AsKeyword
                    | SyntaxKind::MCFNKeyword
                    | SyntaxKind::StructKeyword
                    | SyntaxKind::BreakKeyword
                    | SyntaxKind::ContinueKeyword
                    | SyntaxKind::AppendKeyword
                    | SyntaxKind::StopwatchKeyword
                    | SyntaxKind::CreateKeyword
                    | SyntaxKind::QueryKeyword
                    | SyntaxKind::RestartKeyword
                    | SyntaxKind::RemoveKeyword => Some(SemanticTokenType::Keyword),
                    SyntaxKind::StructName => Some(SemanticTokenType::Struct),
                    SyntaxKind::StructFieldName
                    | SyntaxKind::FieldName
                    | SyntaxKind::BindingPatternName
                    | SyntaxKind::NamedNBTPathNodeName => Some(SemanticTokenType::Variable),
                    SyntaxKind::DataTypeName | SyntaxKind::RuntimeStorageType => {
                        Some(SemanticTokenType::Class)
                    }
                    _ => None,
                };

                if let Some(semantic_token_type) = semantic_token_type {
                    tokens.push(SemanticToken::new(
                        text_range_to_span(token.text_range()),
                        semantic_token_type,
                    ));
                }
            }
        }
    }

    tokens
}
