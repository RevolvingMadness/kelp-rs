use kelp_core::span::Span;

use crate::{
    cstlib::{CSTNodeType, error::CSTError, node::CSTNode, token::CSTToken},
    lower::root::CSTRoot,
    syntax::SyntaxKind,
};

#[derive(Debug)]
pub struct ParseResult {
    pub root: CSTNodeType,
    pub errors: Vec<CSTError>,
}

#[derive(Debug, Clone)]
pub enum Event<'a> {
    StartNode(SyntaxKind),
    FinishNode,
    Token { kind: SyntaxKind, text: &'a str },
    Error { message: String, len: usize },
}

pub struct Parser<'a> {
    pub source: &'a str,
    pub pos: usize,
    events: Vec<Event<'a>>,
    error_count: usize,
    max_errors: usize,
}

impl<'a> Parser<'a> {
    #[must_use]
    pub const fn new(source: &'a str) -> Self {
        Self {
            source,
            pos: 0,
            events: Vec::new(),
            error_count: 0,
            max_errors: 100,
        }
    }

    #[must_use]
    pub fn peek(&self) -> Option<char> {
        self.source[self.pos..].chars().next()
    }

    #[must_use]
    pub fn peek_char(&self) -> Option<char> {
        self.source[self.pos..].chars().next()
    }

    #[must_use]
    pub fn peek_nth_char(&self, n: usize) -> Option<char> {
        self.source[self.pos..].chars().nth(n)
    }

    #[must_use]
    pub fn _peek_whole_value(&self) -> Option<&'a str> {
        let s = &self.source[self.pos..];
        let mut len = 0;
        let mut chars = s.chars();

        while let Some(char) = chars.next()
            && char.is_ascii_digit()
        {
            len += char.len_utf8();
        }

        if len > 0 {
            Some(&self.source[self.pos..self.pos + len])
        } else {
            None
        }
    }

    #[must_use]
    pub fn peek_fractional_value(&self) -> Option<(bool, &'a str)> {
        let s = &self.source[self.pos..];
        let mut len = 0;
        let chars = s.chars();
        let mut has_dot = false;

        for c in chars {
            if c.is_ascii_digit() {
                len += c.len_utf8();
            } else if c == '.' && !has_dot {
                has_dot = true;
                len += c.len_utf8();
            } else {
                break;
            }
        }

        if len > 0 {
            Some((has_dot, &self.source[self.pos..self.pos + len]))
        } else {
            None
        }
    }

    pub fn expect_fractional_value(&mut self, message: &str) -> bool {
        if let Some((_, text)) = self.peek_fractional_value() {
            self.add_token(SyntaxKind::FractionalValue, text.len());
            true
        } else {
            self.error(message);
            false
        }
    }

    pub fn try_parse_fractional_value(&mut self) -> bool {
        if let Some((_, text)) = self.peek_fractional_value() {
            self.add_token(SyntaxKind::FractionalValue, text.len());

            true
        } else {
            false
        }
    }

    #[must_use]
    pub fn peek_identifier(&self) -> Option<&'a str> {
        let s = &self.source[self.pos..];
        let mut chars = s.chars();

        let first = chars.next()?;
        if !(first.is_alphabetic() || first == '_') {
            return None;
        }

        let mut end = self.pos + first.len_utf8();

        for c in chars {
            if c.is_alphanumeric() || c == '_' {
                end += c.len_utf8();
            } else {
                break;
            }
        }

        Some(&self.source[self.pos..end])
    }

    pub fn peek_quoted_char(&mut self) -> Option<&'a str> {
        let s = &self.source[self.pos..];
        let mut chars = s.chars();

        let first_char = chars.next()?;
        if first_char != '\'' {
            return None;
        }

        let start = self.pos;
        let mut end = start + 1;
        let mut escaped = false;

        for char in chars {
            end += char.len_utf8();

            if escaped {
                escaped = false;
            } else if char == '\\' {
                escaped = true;
            } else if char == '\'' {
                return Some(&self.source[start..(end)]);
            }
        }

        self.error("Unterminated character literal");

        Some(&self.source[start..(end)])
    }

    pub fn peek_quoted_string(&mut self) -> Option<&'a str> {
        let s = &self.source[self.pos..];
        let mut chars = s.chars();

        let first_char = chars.next()?;
        if first_char != '"' {
            return None;
        }

        let start = self.pos;
        let mut end = start + 1;
        let mut escaped = false;

        for char in chars {
            end += char.len_utf8();

            if escaped {
                escaped = false;
            } else if char == '\\' {
                escaped = true;
            } else if char == '"' {
                return Some(&self.source[start..end]);
            }
        }

        self.error("Unterminated string literal");

        Some(&self.source[start..end])
    }

    #[must_use]
    pub const fn is_eof(&self) -> bool {
        self.pos >= self.source.len()
    }

    pub fn advance_char(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.pos += c.len_utf8();
        Some(c)
    }

    pub fn start_node(&mut self, kind: SyntaxKind) {
        self.events.push(Event::StartNode(kind));
    }

    pub fn finish_node(&mut self) {
        self.events.push(Event::FinishNode);
    }

    pub fn error(&mut self, message: &str) {
        self.error_with_len(message, 1);
    }

    pub fn error_at(&mut self, position: usize, message: &str) {
        self.error_with_len_at(position, message, 1);
    }

    pub fn error_with_len(&mut self, message: &str, len: usize) {
        if self.error_count >= self.max_errors {
            return;
        }

        #[cfg(debug_assertions)]
        println!("Error: {}", message);

        self.events.push(Event::Error {
            message: message.to_string(),
            len,
        });

        self.error_count += 1;
    }

    pub fn error_with_len_at(&mut self, position: usize, message: &str, len: usize) {
        if self.error_count >= self.max_errors {
            return;
        }

        #[cfg(debug_assertions)]
        println!("Error: {}", message);

        self.events.insert(
            position,
            Event::Error {
                message: message.to_string(),
                len,
            },
        );

        self.error_count += 1;
    }

    pub fn add_token(&mut self, kind: SyntaxKind, len: usize) {
        let start = self.pos;
        self.pos += len;
        let text = &self.source[start..self.pos];
        self.events.push(Event::Token { kind, text });
    }

    #[must_use]
    pub fn build_tree(&'a self) -> CSTNodeType {
        let mut stack = vec![(None, 0usize, Vec::new())];

        let mut current_offset = 0usize;

        for event in &self.events {
            match event {
                Event::StartNode(kind) => {
                    stack.push((Some(kind), current_offset, Vec::new()));
                }
                Event::Token { kind, text } => {
                    let start = current_offset;
                    current_offset += text.len();

                    stack
                        .last_mut()
                        .unwrap()
                        .2
                        .push(CSTNodeType::Token(CSTToken {
                            kind: *kind,
                            span: Span {
                                start,
                                end: current_offset,
                            },
                        }));
                }
                Event::Error { message, len } => {
                    stack
                        .last_mut()
                        .unwrap()
                        .2
                        .push(CSTNodeType::Error(CSTError {
                            message: message.clone(),
                            span: Span {
                                start: current_offset,
                                end: current_offset + len,
                            },
                        }));
                }
                Event::FinishNode => {
                    let (kind, start_offset, children) = stack.pop().unwrap();

                    let node = CSTNodeType::Node(CSTNode {
                        kind: *kind.unwrap(),
                        children,
                        span: Span {
                            start: start_offset,
                            end: current_offset,
                        },
                    });
                    stack.last_mut().unwrap().2.push(node);
                }
            }
        }

        let (_, _, nodes) = stack.pop().unwrap();

        nodes.into_iter().next().unwrap()
    }

    #[must_use]
    pub const fn save_state(&self) -> (usize, usize) {
        (self.pos, self.events.len())
    }

    pub fn restore_state(&mut self, state: (usize, usize)) {
        self.pos = state.0;
        self.events.truncate(state.1);
    }
}

impl Parser<'_> {
    fn skip_whitespace_internal(&mut self, stop_at_newline: bool) -> bool {
        let mut contains_newline = false;
        let mut start_pos = self.pos;
        let bytes = self.source.as_bytes();

        while self.pos < bytes.len() {
            let b = bytes[self.pos];

            match b {
                b'/' if self.pos + 1 < bytes.len() => {
                    let next_b = bytes[self.pos + 1];
                    if next_b == b'/' {
                        self.add_whitespace_token(start_pos);
                        let comment_start = self.pos;
                        self.pos += 2;
                        while self.pos < bytes.len() {
                            if bytes[self.pos] == b'\n' {
                                contains_newline = true;
                                break;
                            }
                            self.pos += 1;
                        }
                        self.add_comment_token(comment_start);
                        start_pos = self.pos;
                    } else if next_b == b'*' {
                        self.add_whitespace_token(start_pos);
                        let comment_start = self.pos;
                        self.pos += 2;
                        while self.pos < bytes.len() {
                            if bytes[self.pos] == b'*'
                                && self.pos + 1 < bytes.len()
                                && bytes[self.pos + 1] == b'/'
                            {
                                self.pos += 2;
                                break;
                            }
                            if bytes[self.pos] == b'\n' {
                                contains_newline = true;
                            }
                            self.pos += 1;
                        }
                        self.add_comment_token(comment_start);
                        start_pos = self.pos;
                    } else {
                        break;
                    }
                }
                b'\n' => {
                    if stop_at_newline {
                        break;
                    }
                    contains_newline = true;
                    self.pos += 1;
                }
                b if b.is_ascii_whitespace() => {
                    self.pos += 1;
                }
                _ => {
                    if !b.is_ascii() {
                        let s = &self.source[self.pos..];
                        if let Some(c) = s.chars().next()
                            && c.is_whitespace()
                        {
                            self.pos += c.len_utf8();
                            continue;
                        }
                    }
                    break;
                }
            }
        }

        self.add_whitespace_token(start_pos);
        contains_newline
    }

    fn add_whitespace_token(&mut self, start: usize) {
        if self.pos > start {
            let text = &self.source[start..self.pos];
            self.events.push(Event::Token {
                kind: SyntaxKind::Whitespace,
                text,
            });
        }
    }

    fn add_comment_token(&mut self, start: usize) {
        let text = &self.source[start..self.pos];

        self.events.push(Event::Token {
            kind: SyntaxKind::Comment,
            text,
        });
    }

    pub fn skip_whitespace(&mut self) {
        self.skip_whitespace_internal(false);
    }

    pub fn skip_inline_whitespace(&mut self) {
        self.skip_whitespace_internal(true);
    }

    #[inline]
    pub fn expect_inline_whitespace(&mut self) -> bool {
        self.expect_inline_whitespace_if(true)
    }

    pub fn expect_inline_whitespace_if(&mut self, display_message: bool) -> bool {
        let start_pos = self.pos;

        self.skip_whitespace_internal(true);

        if self.pos > start_pos {
            true
        } else {
            if display_message {
                self.error("Expected inline whitespace");
            }

            false
        }
    }

    pub fn expect_newline_whitespace(&mut self, message: &str) -> bool {
        let start = self.pos;
        let found_newline = self.skip_whitespace_internal(false);
        if !found_newline && !self.is_eof() {
            self.error_with_len(message, self.pos - start);
            return false;
        }
        true
    }

    pub fn parse(&mut self) -> ParseResult {
        CSTRoot::parse(self);

        let root = self.build_tree();
        let mut errors = Vec::new();
        Self::extract_errors(&mut errors, &root);

        ParseResult { root, errors }
    }

    fn extract_errors(errors: &mut Vec<CSTError>, node: &CSTNodeType) {
        match node {
            CSTNodeType::Node(node) => {
                for child in &node.children {
                    Self::extract_errors(errors, child);
                }
            }
            CSTNodeType::Error(error) => errors.push(error.clone()),
            CSTNodeType::Token(_) => {}
        }
    }

    pub fn bump_until_newline(&mut self) {
        let bytes = &self.source.as_bytes()[self.pos..];

        let length = bytes
            .iter()
            .position(|&b| b == b'\n')
            .unwrap_or(bytes.len());

        self.add_token(SyntaxKind::Garbage, length);
    }

    pub fn recover_newline(&mut self, message: &str) {
        self.error(message);

        self.bump_until_newline();
    }

    pub fn try_parse_string_or_identifier(&mut self) -> bool {
        if self.peek_char() == Some('"') {
            let text = self.peek_quoted_string().unwrap();
            self.add_token(SyntaxKind::String, text.len());
            true
        } else if let Some(ident) = self.peek_identifier() {
            self.bump_identifier(ident);
            true
        } else {
            false
        }
    }

    pub fn parse_range(&mut self) {
        self.start_node(SyntaxKind::Range);

        let checkpoint = self.checkpoint();
        if self.try_parse_fractional_value() {
            self.start_node_at(checkpoint, SyntaxKind::RangeBound);
            self.finish_node();
        }

        if self.peek_char() == Some('.') && self.peek_nth_char(1) == Some('.') {
            self.bump_char();
            self.bump_char();

            let checkpoint = self.checkpoint();
            if self.try_parse_fractional_value() {
                self.start_node_at(checkpoint, SyntaxKind::RangeBound);
                self.finish_node();
            }
        }

        self.finish_node();
    }

    pub fn try_bump_char(&mut self, expected: char) -> bool {
        if self.peek_char() == Some(expected) {
            self.add_token(Self::char_to_kind(expected), expected.len_utf8());

            true
        } else {
            false
        }
    }

    pub fn expect_char(&mut self, expected: char, message: &str) -> bool {
        if self.try_bump_char(expected) {
            true
        } else {
            self.error(message);
            false
        }
    }

    pub fn expect_identifier(&mut self, message: &str) -> bool {
        if let Some(text) = self.peek_identifier() {
            self.add_token(SyntaxKind::Identifier, text.len());

            true
        } else {
            self.error(message);
            false
        }
    }

    pub fn try_start_node_bump(&mut self, char: char, node_kind: SyntaxKind) -> bool {
        if self.peek_char() == Some(char) {
            self.start_node(node_kind);

            self.add_token(Self::char_to_kind(char), char.len_utf8());

            true
        } else {
            false
        }
    }

    pub fn expect_no_bump(&mut self, char: char, message: &str) -> bool {
        if self.peek_char() == Some(char) {
            true
        } else {
            self.error(message);
            false
        }
    }

    pub fn start_bump_finish_node(&mut self, kind: SyntaxKind, len: usize) {
        self.start_node(kind);
        self.add_token(kind, len);
        self.finish_node();
    }

    pub fn start_node_bump(&mut self, kind: SyntaxKind, len: usize) {
        self.start_node(kind);
        self.add_token(kind, len);
    }

    const fn char_to_kind(c: char) -> SyntaxKind {
        match c {
            '@' => SyntaxKind::At,
            '~' => SyntaxKind::Tilde,
            ':' => SyntaxKind::Colon,
            '{' => SyntaxKind::LeftBrace,
            '}' => SyntaxKind::RightBrace,
            '[' => SyntaxKind::LeftBracket,
            ']' => SyntaxKind::RightBracket,
            '(' => SyntaxKind::LeftParenthesis,
            ')' => SyntaxKind::RightParenthesis,
            '=' => SyntaxKind::Equal,
            '!' => SyntaxKind::ExclamationMark,
            ',' => SyntaxKind::Comma,
            '.' => SyntaxKind::Period,
            '\'' => SyntaxKind::SingleQuote,
            '"' => SyntaxKind::DoubleQuote,
            '#' => SyntaxKind::Pound,
            '+' => SyntaxKind::Plus,
            '-' => SyntaxKind::Minus,
            '*' => SyntaxKind::Star,
            '/' => SyntaxKind::ForwardSlash,
            '\\' => SyntaxKind::BackwardSlash,
            '&' => SyntaxKind::Ampersand,
            '%' => SyntaxKind::Percent,
            '$' => SyntaxKind::DollarSign,
            '_' => SyntaxKind::Underscore,
            ';' => SyntaxKind::Semicolon,
            '<' => SyntaxKind::LeftArrow,
            '>' => SyntaxKind::RightArrow,
            '?' => SyntaxKind::QuestionMark,
            '|' => SyntaxKind::Pipe,
            '`' => SyntaxKind::Backtick,
            _ => SyntaxKind::Garbage,
        }
    }

    pub fn bump_until_char(&mut self, stop_chars: &[char]) {
        while let Some(c) = self.peek_char()
            && !stop_chars.contains(&c)
        {
            self.bump_char();
        }
    }

    pub fn bump_char(&mut self) {
        if let Some(char) = self.peek_char() {
            self.add_token(Self::char_to_kind(char), char.len_utf8());
        }
    }

    pub fn bump_char_kind(&mut self, kind: SyntaxKind) {
        if let Some(char) = self.peek_char() {
            self.add_token(kind, char.len_utf8());
        }
    }

    #[inline]
    pub fn bump_identifier(&mut self, identifier: &str) {
        self.add_token(SyntaxKind::Identifier, identifier.len());
    }

    pub fn bump_keyword(&mut self, keyword: &str) {
        self.add_token(SyntaxKind::Keyword, keyword.len());
    }

    #[must_use]
    pub const fn checkpoint(&self) -> usize {
        self.events.len()
    }

    pub fn start_node_at(&mut self, checkpoint: usize, kind: SyntaxKind) {
        self.events.insert(checkpoint, Event::StartNode(kind));
    }
}
