use kelp_core::span::Span;

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
