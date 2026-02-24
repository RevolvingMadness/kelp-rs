use kelp_core::{data_type::high::HighDataType, span::Span};

use crate::{
    cst_node,
    lower::{data_type::CSTDataType, expression::r#struct::CSTStructExpression},
    parser::Parser,
    syntax::SyntaxKind,
};

cst_node!(
    CSTStructDeclarationField,
    SyntaxKind::StructDeclarationField
);

impl<'a> CSTStructDeclarationField<'a> {
    pub fn lower(self, text: &str) -> Option<(String, HighDataType)> {
        let name = self.name(text)?;

        let data_type = self.data_type()?.lower(text)?;

        Some((name.to_string(), data_type))
    }

    pub fn name_span(&self) -> Option<Span> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some(token.span)
            } else {
                None
            }
        })
    }

    pub fn name<'b>(&self, text: &'b str) -> Option<&'b str> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some(token.text(text))
            } else {
                None
            }
        })
    }

    pub fn data_type(&self) -> Option<CSTDataType<'a>> {
        self.children().find_map(CSTDataType::cast)
    }
}

cst_node!(
    CSTStructDeclarationStatement,
    SyntaxKind::StructDeclarationStatement
);

impl<'a> CSTStructDeclarationStatement<'a> {
    fn bump_until_next_generic_parameter(parser: &mut Parser) {
        let chars = parser.source[parser.pos..].chars();
        let mut length = 0;

        for char in chars {
            if char == ',' || char == '>' || char.is_alphabetic() {
                break;
            }

            length += char.len_utf8();
        }

        if length > 0 {
            parser.add_token(SyntaxKind::Garbage, length);
        }

        if parser.peek_char() == Some(',') {
            parser.bump_char();
        }
    }

    pub fn try_parse(parser: &mut Parser) -> bool {
        let beginning = parser.save_state();

        parser.start_node(SyntaxKind::StructDeclarationStatement);
        parser.bump_keyword("struct");
        parser.expect_inline_whitespace();

        if !parser.expect_identifier("Expected struct name") {
            parser.restore_state(beginning);

            return false;
        }

        parser.skip_whitespace();

        if parser.try_bump_char('<') {
            loop {
                parser.skip_whitespace();

                if parser.is_eof() || matches!(parser.peek_char(), Some('>' | '{' | '}')) {
                    break;
                }

                if !parser.expect_identifier("Expected generic parameter") {
                    Self::bump_until_next_generic_parameter(parser);

                    continue;
                }

                parser.skip_whitespace();

                if !parser.try_bump_char(',')
                    && !matches!(parser.peek_char(), Some('>' | '{' | '}' | ','))
                {
                    parser.error("Expected ',' or '>'");

                    Self::bump_until_next_generic_parameter(parser);
                }
            }

            parser.expect_char('>', "Expected '>'");

            parser.skip_whitespace();
        }

        if !parser.expect_char('{', "Expected '{'") {
            CSTStructExpression::bump_until_next_field_or_end(parser);
        }

        loop {
            parser.skip_whitespace();

            if parser.is_eof() || parser.peek_char() == Some('}') {
                break;
            }

            parser.start_node(SyntaxKind::StructDeclarationField);

            if !parser.expect_identifier("Expected struct field name") {
                CSTStructExpression::bump_until_next_field_or_end(parser);

                parser.finish_node();

                continue;
            }

            parser.skip_whitespace();

            let parsed_colon = parser.expect_char(':', "Expected ':'");

            parser.skip_whitespace();

            if !CSTDataType::try_parse(parser) {
                if parsed_colon {
                    parser.error("Expected data type");
                }

                CSTStructExpression::bump_until_next_field_or_end(parser);

                parser.finish_node();

                continue;
            }

            parser.finish_node();
            parser.skip_whitespace();

            if !parser.try_bump_char(',') && parser.peek_char() != Some('}') {
                parser.error("Expected ',' or '}'");
            }
        }

        parser.expect_char('}', "Expected '}'");

        parser.finish_node();

        true
    }

    pub fn struct_keyword_span(&self) -> Option<Span> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Keyword {
                Some(token.span)
            } else {
                None
            }
        })
    }

    pub fn name<'b>(&self, text: &'b str) -> Option<&'b str> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::Identifier {
                Some(token.text(text))
            } else {
                None
            }
        })
    }

    pub fn generics_span(&self) -> impl Iterator<Item = Span> {
        self.0
            .children_tokens()
            .filter_map(|token| {
                if token.kind == SyntaxKind::Identifier {
                    Some(token.span)
                } else {
                    None
                }
            })
            .skip(1)
    }

    pub fn generics<'b>(&self, text: &'b str) -> impl Iterator<Item = &'b str> {
        self.0
            .children_tokens()
            .filter_map(|token| {
                if token.kind == SyntaxKind::Identifier {
                    Some(token.text(text))
                } else {
                    None
                }
            })
            .skip(1)
    }

    pub fn fields(&self) -> impl Iterator<Item = CSTStructDeclarationField<'a>> {
        self.0
            .children()
            .filter_map(CSTStructDeclarationField::cast)
    }
}
