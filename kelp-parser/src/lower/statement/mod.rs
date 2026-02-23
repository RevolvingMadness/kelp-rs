use kelp_core::{
    span::Span,
    statement::{Statement, StatementKind},
};

use crate::{
    cstlib::CSTNodeType,
    lower::{
        data_type::CSTDataType,
        expression::CSTExpression,
        statement::{
            block::CSTBlockStatement, r#if::CSTIfStatement, r#let::CSTLetStatement,
            mcfn_declaration::CSTMCFNDeclarationStatement, r#while::CSTWhileStatement,
        },
    },
    parser::Parser,
    syntax::SyntaxKind,
};

pub mod block;
pub mod r#if;
pub mod r#let;
pub mod mcfn_declaration;
pub mod r#while;

#[derive(Debug)]
pub enum CSTStatementKind<'a> {
    MCFNDeclaration(CSTMCFNDeclarationStatement<'a>),
    If(CSTIfStatement<'a>),
    While(CSTWhileStatement<'a>),
    Let(CSTLetStatement<'a>),
    Expression(CSTExpression<'a>),
    Block(CSTBlockStatement<'a>),
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
                        "mcfn" => {
                            if CSTMCFNDeclarationStatement::try_parse(parser) {
                                return true;
                            }
                        }
                        "if" => {
                            if CSTIfStatement::try_parse(parser) {
                                return true;
                            }
                        }
                        "while" => {
                            if CSTWhileStatement::try_parse(parser) {
                                return true;
                            }
                        }
                        "let" => {
                            if CSTLetStatement::try_parse(parser) {
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

    pub fn cast(node: &'a CSTNodeType) -> Option<CSTStatement<'a>> {
        Some(
            (match node.kind()? {
                SyntaxKind::MCFNDeclarationStatement => {
                    CSTStatementKind::MCFNDeclaration(CSTMCFNDeclarationStatement::cast(node)?)
                }
                SyntaxKind::IfStatement => CSTStatementKind::If(CSTIfStatement::cast(node)?),
                SyntaxKind::WhileStatement => {
                    CSTStatementKind::While(CSTWhileStatement::cast(node)?)
                }
                SyntaxKind::LetStatement => CSTStatementKind::Let(CSTLetStatement::cast(node)?),
                SyntaxKind::Block => CSTStatementKind::Block(CSTBlockStatement::cast(node)?),
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

    pub fn lower(self) -> Option<Statement> {
        Some(
            (match self.kind {
                CSTStatementKind::MCFNDeclaration(statement) => {
                    let resource_location = statement.resource_location()?.lower()?;

                    let statement = statement.block_statement()?.lower()?;

                    StatementKind::MCFunction(resource_location, Box::new(statement))
                }
                CSTStatementKind::If(statement) => {
                    let condition = statement.condition()?.lower()?;
                    let body = statement.body()?.lower()?;
                    let else_body = statement
                        .else_body()
                        .and_then(CSTStatement::lower)
                        .map(Box::new);

                    StatementKind::If(condition, Box::new(body), else_body)
                }
                CSTStatementKind::While(statement) => {
                    let condition = statement.condition()?.lower()?;
                    let body = statement.body()?.lower()?;

                    StatementKind::While(condition, Box::new(body))
                }
                CSTStatementKind::Let(statement) => {
                    let pattern = statement.pattern()?.lower()?;

                    let data_type = statement.data_type().and_then(CSTDataType::lower);

                    let value = statement.value()?.lower()?;

                    StatementKind::VariableDeclaration(data_type, pattern, value)
                }
                CSTStatementKind::Block(statement) => {
                    let statements = statement
                        .statements()
                        .filter_map(CSTStatement::lower)
                        .collect();

                    StatementKind::Block(statements)
                }
                CSTStatementKind::Expression(expression) => {
                    StatementKind::Expression(expression.lower()?)
                }
            })
            .with_span(self.span),
        )
    }
}
