use kelp_core::span::Span;

#[derive(Debug)]
pub struct SemanticToken {
    pub span: Span,
    pub type_: SemanticTokenType,
    pub modifiers: Vec<SemanticTokenModifier>,
}

impl SemanticToken {
    #[inline]
    #[must_use]
    pub fn new(span: Span, type_: SemanticTokenType) -> SemanticToken {
        SemanticToken {
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
    pub fn to_index(self) -> u32 {
        match self {
            SemanticTokenType::Namespace => 0,
            SemanticTokenType::Type => 1,
            SemanticTokenType::Class => 2,
            SemanticTokenType::Enum => 3,
            SemanticTokenType::Interface => 4,
            SemanticTokenType::Struct => 5,
            SemanticTokenType::TypeParameter => 6,
            SemanticTokenType::Parameter => 7,
            SemanticTokenType::Variable => 8,
            SemanticTokenType::Property => 9,
            SemanticTokenType::EnumMember => 10,
            SemanticTokenType::Event => 11,
            SemanticTokenType::Function => 12,
            SemanticTokenType::Method => 13,
            SemanticTokenType::Macro => 14,
            SemanticTokenType::Keyword => 15,
            SemanticTokenType::Modifier => 16,
            SemanticTokenType::Comment => 17,
            SemanticTokenType::String => 18,
            SemanticTokenType::Number => 19,
            SemanticTokenType::Regexp => 20,
            SemanticTokenType::Operator => 21,
            SemanticTokenType::Decorator => 22,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum SemanticTokenModifier {}

impl SemanticTokenModifier {
    #[must_use]
    pub fn to_bit(self) -> u32 {
        match self {}
    }
}
