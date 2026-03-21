use rowan::Language;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    Root,

    Identifier,
    StructName,
    StructFieldName,
    DataTypeName,

    WholeValue,
    FractionalValue,

    At,                        // @
    Tilde,                     // ~
    Caret,                     // ^
    Colon,                     // :
    ColonColon,                // ::
    LeftBrace,                 // {
    RightBrace,                // }
    LeftBracket,               // [
    RightBracket,              // ]
    LeftParenthesis,           // (
    RightParenthesis,          // )
    Equal,                     // =
    PlusEqual,                 // +=
    MinusEqual,                // -=
    StarEqual,                 // *=
    ForwardSlashEqual,         // /=
    PercentEqual,              // %=
    AmpersandEqual,            // &=
    PipeEqual,                 // |=
    LeftArrowLeftArrowEqual,   // <<=
    RightArrowRightArrowEqual, // >>=
    RightArrowLeftArrow,       // ><
    ExclamationMark,           // !
    Comma,                     // ,
    Period,                    // .
    SingleQuote,               // '
    DoubleQuote,               // "
    Pound,                     // #
    Plus,                      // +
    Minus,                     // -
    Star,                      // *
    ForwardSlash,              // /
    BackwardSlash,             // \
    Ampersand,                 // &
    Percent,                   // %
    DollarSign,                // $
    Underscore,                // _
    Semicolon,                 // ;
    LeftArrow,                 // <
    LeftArrowLeftArrow,        // <<
    RightArrow,                // >
    RightArrowRightArrow,      // >>
    QuestionMark,              // ?
    Pipe,                      // |
    Backtick,                  // `
    AmpersandAmpersand,        // &&
    PipePipe,                  // ||
    EqualEqual,                // ==
    ExclamationMarkEqual,      // !=
    RightArrowEqual,           // >=
    LeftArrowEqual,            // <=

    PlayerScore,
    ScoreboardObjective,
    Data,

    CharacterLiteral,
    StringLiteral,

    Whitespace,
    Comment,

    LocalCoordinates,
    LocalCoordinate,
    WorldCoordinates,
    WorldCoordinate,

    ResourceLocation,
    ResourceLocationTag,
    ResourceLocationNamespace,
    ResourceLocationPath,
    ResourceLocationPathSegment,

    NBTCompound,
    NBTCompoundEntry,
    NamedNBTPathNode,
    NamedNBTPathNodeName,
    IndexNBTPathNode,
    CompoundNBTPathNode,
    NBTPathNode,
    NBTPath,

    WildcardPattern,
    TuplePattern,
    BindingPattern,
    BindingPatternName,
    ScorePattern,
    DataPattern,
    StructPattern,
    StructPatternField,
    CompoundPattern,
    CompoundPatternEntry,

    ModuleName,

    ScoreKeyword,
    EntityKeyword,
    BlockKeyword,
    StorageKeyword,
    TellrawKeyword,
    FunctionKeyword,
    StopwatchKeyword,
    CreateKeyword,
    QueryKeyword,
    RestartKeyword,
    ReturnKeyword,
    IfKeyword,
    WhileKeyword,
    ForKeyword,
    InKeyword,
    LoopKeyword,
    ElseKeyword,
    LetKeyword,
    ToKeyword,
    AsKeyword,
    ModKeyword,
    MCFNKeyword,
    StructKeyword,
    TypeKeyword,
    BreakKeyword,
    ContinueKeyword,
    AppendKeyword,
    RemoveKeyword,
    TrueKeyword,
    FalseKeyword,

    ModuleDeclarationItem,
    MCFNDeclarationItem,
    StructDeclarationItem,
    StructDeclarationItemField,
    TypeAliasDeclarationItem,

    BlockStatement,
    ExpressionStatement,
    IfStatement,
    LetStatement,
    WhileStatement,
    ForStatement,
    LoopStatement,
    BreakStatement,
    ContinueStatement,
    AppendStatement,
    RemoveStatement,
    ItemStatement,

    GenericNames,
    GenericDataTypes,

    Range,
    RangeBound,

    NameEntitySelector,
    VariableEntitySelector,
    EntitySelectorVariable,
    VariableEntitySelectorOptions,
    VariableEntitySelectorOption,
    VariableEntitySelectorOptionValue,

    BlockDataTarget,
    StorageDataTarget,
    EntityDataTarget,

    RuntimeStorageType,

    TellrawCommandExpression,
    FunctionCommandExpression,
    ReturnCommandExpression,
    StopwatchCommandExpression,
    StopwatchCommandExpressionOptions,
    StopwatchCommandExpressionCreate,
    StopwatchCommandExpressionQuery,
    StopwatchCommandExpressionRemove,
    StopwatchCommandExpressionRestart,

    StructExpression,
    StructExpressionField,
    FieldAccessExpression,
    FieldName,
    AssignmentExpression,
    UnaryExpression,
    PathExpression,
    UnderscoreExpression,
    NumericExpression,
    NumericExpressionSuffix,
    CharacterExpression,
    StringExpression,
    BooleanExpression,
    IndexExpression,
    ListExpression,
    CompoundExpression,
    CompoundExpressionEntry,
    CompoundKey,
    DataExpression,
    ScoreExpression,
    CommandExpression,
    BinaryExpression,
    UnitExpression,
    TupleExpression,
    ToCastExpression,
    AsCastExpression,
    ParenthesizedExpression,

    UnitDataType,
    TupleDataType,
    ReferenceDataType,
    TypedCompoundDataType,
    TypedCompoundDataTypeField,
    TypedCompoundDataTypeFieldName,
    PathDataType,
    InferredDataType,

    Path,
    PathSegment,
    PathIdentifier,

    Error,
    Garbage,
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

impl From<rowan::SyntaxKind> for SyntaxKind {
    fn from(kind: rowan::SyntaxKind) -> Self {
        unsafe { std::mem::transmute(kind.0) }
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct KelpLanguage;

impl Language for KelpLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        raw.into()
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

pub type SyntaxNode = rowan::SyntaxNode<KelpLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<KelpLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<KelpLanguage>;
