#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    Root,
    Identifier,
    Keyword,
    Integer,
    Float,
    At,                        // @
    Tilde,                     // ~
    Caret,                     // ^
    Colon,                     // :
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
    Char,
    String,

    Whitespace,
    Comment,

    LocalCoordinates,
    LocalCoordinate,
    WorldCoordinates,
    WorldCoordinate,

    ResourceLocation,
    ResourceLocationTag,
    ResourceLocationNamespace,
    ResourceLocationPaths,

    NBTCompound,
    NBTCompoundEntry,
    NBTPathNamed,
    NBTPathIndex,
    NBTPathRoot,
    NBTPath,

    WildcardPattern,
    TuplePattern,
    BindingPattern,
    StructPattern,
    StructPatternField,

    IfStatement,
    WhileStatement,
    LetStatement,
    MCFNDeclarationStatement,
    Block,

    Range,
    RangeBound,

    EntitySelectorName,
    VariableEntitySelector,
    EntitySelectorVariable,
    VariableEntitySelectorOptions,
    VariableEntitySelectorOption,
    VariableEntitySelectorOptionValue,

    BlockDataTarget,
    StorageDataTarget,
    EntityDataTarget,

    StructField,

    StructExpression,
    FieldAccessExpression,
    AssignmentExpression,
    UnaryExpression,
    LiteralExpression,
    BooleanExpression,
    IndexExpression,
    ListExpression,
    CompoundExpression,
    CompoundExpressionEntry,
    DataExpression,
    ScoreExpression,
    TellrawCommandExpression,
    BinaryExpression,
    UnitExpression,
    TupleExpression,
    ToCastExpression,
    RuntimeStorageType,
    AsCastExpression,
    ParenthesizedExpression,

    UnitDataType,
    TupleDataType,
    ReferenceDataType,
    TypedCompoundDataType,
    TypedCompoundField,
    NamedDataType,

    Error,
    Garbage,
}
