use kelp_parser::{cstlib::CSTNodeType, syntax::SyntaxKind};
use tower_lsp::lsp_types::SemanticToken;

use crate::LineIndex;

fn kind_to_token_type(kind: SyntaxKind) -> Option<u32> {
    match kind {
        SyntaxKind::Keyword => Some(0),

        SyntaxKind::TellrawCommandExpression => Some(0),

        SyntaxKind::Identifier => Some(2),
        SyntaxKind::Integer | SyntaxKind::Float => Some(4),
        SyntaxKind::Char => Some(3),
        SyntaxKind::Plus | SyntaxKind::Minus | SyntaxKind::Equal => Some(5),
        _ => None,
    }
}

fn get_semantic_tokens(node: &CSTNodeType, line_index: &LineIndex) -> Vec<SemanticToken> {
    let mut tokens = Vec::new();
    let mut last_line = 0;
    let mut last_start = 0;

    fn traverse(
        node: &CSTNodeType,
        tokens: &mut Vec<SemanticToken>,
        last_line: &mut u32,
        last_start: &mut u32,
        line_index: &LineIndex,
    ) {
        match node {
            CSTNodeType::Token(token) => {
                if let Some(token_type) = kind_to_token_type(token.kind) {
                    let (line, col) = line_index.offset_to_line_col(token.span.start);

                    let delta_line = line - *last_line;
                    let delta_start = if delta_line == 0 {
                        col - *last_start
                    } else {
                        col
                    };

                    tokens.push(SemanticToken {
                        delta_line,
                        delta_start,
                        length: token.text.len() as u32,
                        token_type,
                        token_modifiers_bitset: 0,
                    });

                    *last_line = line;
                    *last_start = col;
                }
            }
            CSTNodeType::Node(n) => {
                for child in &n.children {
                    traverse(child, tokens, last_line, last_start, line_index);
                }
            }
            _ => {}
        }
    }

    traverse(
        node,
        &mut tokens,
        &mut last_line,
        &mut last_start,
        line_index,
    );

    tokens
}
