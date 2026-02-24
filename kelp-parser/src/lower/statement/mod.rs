use kelp_core::{
    span::Span,
    statement::{Statement, StatementKind},
};

use crate::{
    cstlib::CSTNodeType,
    lower::{
        expression::CSTExpression,
        statement::{
            block::CSTBlockStatement, r#if::CSTIfStatement, r#let::CSTLetStatement,
            mcfn_declaration::CSTMCFNDeclarationStatement,
            struct_declaration::CSTStructDeclarationStatement, r#while::CSTWhileStatement,
        },
    },
    parser::Parser,
    semantic_token::{SemanticToken, SemanticTokenType},
    syntax::SyntaxKind,
};

pub mod block;
pub mod r#if;
pub mod r#let;
pub mod mcfn_declaration;
pub mod struct_declaration;
pub mod r#while;

#[derive(Debug)]
pub enum CSTStatementKind<'a> {
    Block(CSTBlockStatement<'a>),
    Expression(CSTExpression<'a>),
    If(CSTIfStatement<'a>),
    Let(CSTLetStatement<'a>),
    MCFNDeclaration(CSTMCFNDeclarationStatement<'a>),
    StructDeclaration(CSTStructDeclarationStatement<'a>),
    While(CSTWhileStatement<'a>),
}

impl<'a> CSTStatementKind<'a> {
    #[inline]
    #[must_use]
    pub fn with_span(self, span: Span) -> CSTStatement<'a> {
        CSTStatement { span, kind: self }
    }
}

#[derive(Debug)]
pub struct CSTStatement<'a> {
    pub span: Span,
    pub kind: CSTStatementKind<'a>,
}

impl<'a> CSTStatement<'a> {
    #[must_use]
    pub fn is_recovery(char: char) -> bool {
        CSTExpression::is_recovery(char) || char == '\n' || char == '{'
    }

    #[must_use]
    pub fn try_parse(parser: &mut Parser) -> bool {
        let Some(c) = parser.peek_char() else {
            return false;
        };

        match c {
            '{' => CSTBlockStatement::try_parse(parser),
            _ => {
                if let Some(text) = parser.peek_identifier() {
                    match text {
                        "if" => {
                            if CSTIfStatement::try_parse(parser) {
                                return true;
                            }
                        }
                        "let" => {
                            if CSTLetStatement::try_parse(parser) {
                                return true;
                            }
                        }
                        "mcfn" => {
                            if CSTMCFNDeclarationStatement::try_parse(parser) {
                                return true;
                            }
                        }
                        "struct" => {
                            if CSTStructDeclarationStatement::try_parse(parser) {
                                return true;
                            }
                        }
                        "while" => {
                            if CSTWhileStatement::try_parse(parser) {
                                return true;
                            }
                        }
                        _ => {}
                    }
                }

                if !CSTExpression::try_parse(parser) {
                    let chars = parser.source[parser.pos..].chars();
                    let mut length = 0;

                    for char in chars {
                        if CSTStatement::is_recovery(char) {
                            break;
                        }

                        length += char.len_utf8();
                    }

                    if length > 0 {
                        parser.error_with_len("Expected statement", length);

                        parser.add_token(SyntaxKind::Garbage, length);
                    }
                }

                true
            }
        }
    }

    #[must_use]
    pub fn cast(node: &'a CSTNodeType) -> Option<CSTStatement<'a>> {
        Some(
            (match node.kind()? {
                SyntaxKind::BlockStatement => {
                    CSTStatementKind::Block(CSTBlockStatement::cast(node)?)
                }
                SyntaxKind::IfStatement => CSTStatementKind::If(CSTIfStatement::cast(node)?),
                SyntaxKind::LetStatement => CSTStatementKind::Let(CSTLetStatement::cast(node)?),
                SyntaxKind::MCFNDeclarationStatement => {
                    CSTStatementKind::MCFNDeclaration(CSTMCFNDeclarationStatement::cast(node)?)
                }
                SyntaxKind::StructDeclarationStatement => {
                    CSTStatementKind::StructDeclaration(CSTStructDeclarationStatement::cast(node)?)
                }
                SyntaxKind::WhileStatement => {
                    CSTStatementKind::While(CSTWhileStatement::cast(node)?)
                }
                _ => {
                    if let Some(expression) = CSTExpression::cast(node) {
                        CSTStatementKind::Expression(expression)
                    } else {
                        #[cfg(debug_assertions)]
                        println!("Failed to cast node {:?} to CSTStatement", node);

                        return None;
                    }
                }
            })
            .with_span(node.span()),
        )
    }

    pub fn lower(self, text: &str) -> Option<Statement> {
        Some(
            (match self.kind {
                CSTStatementKind::Block(statement) => {
                    let statements = statement
                        .statements()
                        .filter_map(|statement| statement.lower(text))
                        .collect();

                    StatementKind::Block(statements)
                }
                CSTStatementKind::Expression(expression) => {
                    StatementKind::Expression(expression.lower(text)?)
                }
                CSTStatementKind::If(statement) => {
                    let condition = statement.condition()?.lower(text)?;
                    let body = statement.body()?.lower(text)?;
                    let else_body = statement
                        .else_body()
                        .and_then(|statement| statement.lower(text))
                        .map(Box::new);

                    StatementKind::If(condition, Box::new(body), else_body)
                }
                CSTStatementKind::Let(statement) => {
                    let pattern = statement.pattern()?.lower(text)?;

                    let data_type = statement
                        .data_type()
                        .and_then(|data_type| data_type.lower(text));

                    let value = statement.value()?.lower(text)?;

                    StatementKind::VariableDeclaration(data_type, pattern, value)
                }
                CSTStatementKind::MCFNDeclaration(statement) => {
                    let resource_location = statement.resource_location()?.lower(text)?;

                    let statement = statement.block_statement()?.lower(text)?;

                    StatementKind::MCFNDeclaration(resource_location, Box::new(statement))
                }
                CSTStatementKind::StructDeclaration(statement) => {
                    let name = statement.name(text)?.to_string();

                    let generics = statement.generics(text).map(ToString::to_string).collect();

                    let fields = statement
                        .fields()
                        .filter_map(|struct_declaration_field| struct_declaration_field.lower(text))
                        .collect();

                    StatementKind::StructDeclaration(name, generics, fields)
                }
                CSTStatementKind::While(statement) => {
                    let condition = statement.condition()?.lower(text)?;
                    let body = statement.body()?.lower(text)?;

                    StatementKind::While(condition, Box::new(body))
                }
            })
            .with_span(self.span),
        )
    }

    pub fn collect_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        match &self.kind {
            CSTStatementKind::Block(statement) => {
                for statement in statement.statements() {
                    statement.collect_semantic_tokens(tokens);
                }
            }
            CSTStatementKind::Expression(statement) => {
                statement.collect_semantic_tokens(tokens);
            }
            CSTStatementKind::If(statement) => {
                if let Some(if_keyword_span) = statement.if_keyword_span() {
                    tokens.push(SemanticToken::new(
                        if_keyword_span,
                        SemanticTokenType::Keyword,
                    ));
                }

                if let Some(condition) = statement.condition() {
                    condition.collect_semantic_tokens(tokens);
                }

                if let Some(body) = statement.body() {
                    body.collect_semantic_tokens(tokens);
                }

                if let Some(else_keyword_span) = statement.else_keyword_span() {
                    tokens.push(SemanticToken::new(
                        else_keyword_span,
                        SemanticTokenType::Keyword,
                    ));
                }

                if let Some(else_body) = statement.else_body() {
                    else_body.collect_semantic_tokens(tokens);
                }
            }
            CSTStatementKind::Let(statement) => {
                if let Some(let_keyword_span) = statement.let_keyword_span() {
                    tokens.push(SemanticToken::new(
                        let_keyword_span,
                        SemanticTokenType::Keyword,
                    ));
                }

                if let Some(pattern) = statement.pattern() {
                    pattern.collect_semantic_tokens(tokens);
                }

                if let Some(value) = statement.value() {
                    value.collect_semantic_tokens(tokens);
                }
            }
            CSTStatementKind::MCFNDeclaration(statement) => {
                if let Some(mcfn_keyword_span) = statement.mcfn_keyword_span() {
                    tokens.push(SemanticToken::new(
                        mcfn_keyword_span,
                        SemanticTokenType::Keyword,
                    ));
                }

                if let Some(resource_location) = statement.resource_location() {
                    resource_location.collect_semantic_tokens(tokens);
                }

                if let Some(block_statement) = statement.block_statement() {
                    block_statement.collect_semantic_tokens(tokens);
                }
            }
            CSTStatementKind::StructDeclaration(statement) => {
                if let Some(struct_keyword_span) = statement.struct_keyword_span() {
                    tokens.push(SemanticToken::new(
                        struct_keyword_span,
                        SemanticTokenType::Keyword,
                    ));
                }

                for generic_span in statement.generics_span() {
                    tokens.push(SemanticToken::new(generic_span, SemanticTokenType::Class));
                }

                for field in statement.fields() {
                    if let Some(name_span) = field.name_span() {
                        tokens.push(SemanticToken::new(name_span, SemanticTokenType::Variable));
                    }

                    if let Some(data_type) = field.data_type() {
                        data_type.collect_semantic_tokens(tokens);
                    }
                }
            }
            CSTStatementKind::While(statement) => {
                if let Some(while_keyword_span) = statement.while_keyword_span() {
                    tokens.push(SemanticToken::new(
                        while_keyword_span,
                        SemanticTokenType::Keyword,
                    ));
                }

                if let Some(condition) = statement.condition() {
                    condition.collect_semantic_tokens(tokens);
                }

                if let Some(body) = statement.body() {
                    body.collect_semantic_tokens(tokens);
                }
            }
        }
    }
}
