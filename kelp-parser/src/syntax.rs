#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    Root,
    Identifier,
    Keyword,
    WholeValue,
    FractionalValue,
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

    BlockStatement,
    IfStatement,
    LetStatement,
    MCFNDeclarationStatement,
    StructDeclarationStatement,
    StructDeclarationField,
    WhileStatement,

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

    StructExpression,
    StructExpressionField,
    FieldAccessExpression,
    AssignmentExpression,
    UnaryExpression,
    VariableExpression,
    ByteExpression,
    ShortExpression,
    IntegerExpression,
    LongExpression,
    FloatExpression,
    DoubleExpression,
    NumericExpressionSuffix,
    CharExpression,
    StringExpression,
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
