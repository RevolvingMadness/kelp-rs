use kelp_core::semantic_analysis_context::{
    Scope, SemanticAnalysisContext, SemanticAnalysisInfoKind,
};
use kelp_core::span::Span;
use kelp_parser::lower::Lowerer;
use kelp_parser::parser::{ParseResult, Parser};
use std::collections::BTreeMap;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

pub mod cst;

const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::NAMESPACE,
    SemanticTokenType::CLASS,
    SemanticTokenType::ENUM,
    SemanticTokenType::INTERFACE,
    SemanticTokenType::STRUCT,
    SemanticTokenType::TYPE_PARAMETER,
    SemanticTokenType::TYPE,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::ENUM_MEMBER,
    SemanticTokenType::DECORATOR,
    SemanticTokenType::EVENT,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::METHOD,
    SemanticTokenType::MACRO,
    SemanticTokenType::COMMENT,
    SemanticTokenType::STRING,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::NUMBER,
    SemanticTokenType::REGEXP,
    SemanticTokenType::OPERATOR,
];

#[derive(Debug, Clone)]
struct LineIndex {
    text: String,
    line_starts: Vec<usize>,
}

impl LineIndex {
    fn new(text: &str) -> Self {
        let mut line_starts = vec![0];
        line_starts.extend(text.match_indices('\n').map(|(i, _)| i + 1));
        Self {
            text: text.to_string(),
            line_starts,
        }
    }

    fn offset_to_line_col(&self, offset: usize) -> (u32, u32) {
        let clamped_offset = offset.min(self.text.len());

        let line_index = self
            .line_starts
            .binary_search(&clamped_offset)
            .unwrap_or_else(|i| i.saturating_sub(1));

        let line_start_offset = self.line_starts[line_index];

        let character_utf16 = self.text[line_start_offset..clamped_offset]
            .encode_utf16()
            .count();

        (line_index as u32, character_utf16 as u32)
    }

    fn offset_to_position(&self, offset: usize) -> Position {
        let (line, col) = self.offset_to_line_col(offset);

        Position {
            line,
            character: col,
        }
    }

    fn position_to_offset(&self, position: Position) -> Option<usize> {
        let line = position.line as usize;
        if line >= self.line_starts.len() {
            return Some(self.text.len());
        }

        let line_start = self.line_starts[line];
        let line_end = self
            .line_starts
            .get(line + 1)
            .copied()
            .unwrap_or(self.text.len());

        let line_slice = &self.text[line_start..line_end];

        let mut utf16_col = 0;
        let mut byte_pos_in_line = 0;
        for c in line_slice.chars() {
            if utf16_col == position.character {
                return Some(line_start + byte_pos_in_line);
            }
            if utf16_col > position.character {
                return None;
            }
            utf16_col += c.len_utf16() as u32;
            byte_pos_in_line += c.len_utf8();
        }

        if utf16_col == position.character {
            return Some(line_start + byte_pos_in_line);
        }

        None
    }
}

#[derive(Debug, Clone)]
struct DocumentState {
    text: String,
    line_index: LineIndex,
}

#[derive(Debug)]
struct Backend {
    client: Client,
    document_map: RwLock<BTreeMap<Url, DocumentState>>,
}

fn normalize_line_endings(text: &str) -> String {
    text.replace("\r\n", "\n").replace('\r', "\n")
}

impl Backend {
    async fn on_change(&self, uri: Url, text: String) {
        let normalized_text = normalize_line_endings(&text);
        let diagnostics = self.process_text(&uri, normalized_text).await;

        if let Some(diagnostics) = diagnostics {
            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
        }
    }

    async fn process_text(&self, uri: &Url, text: String) -> Option<Vec<Diagnostic>> {
        let mut diagnostics = Vec::new();

        let parser = Parser::new(&text);
        let ParseResult { root, errors } = parser.parse();

        let line_index = LineIndex::new(&text);

        if !errors.is_empty() {
            diagnostics.extend(errors.into_iter().map(|error| Diagnostic {
                range: Range {
                    start: line_index.offset_to_position(error.span.start),
                    end: line_index.offset_to_position(error.span.end),
                },
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("kelp-lsp".to_string()),
                message: error.message.to_string(),
                code_description: None,
                code: None,
                related_information: None,
                tags: None,
                data: None,
            }));
        } else {
            let statements = Lowerer::lower_root(&root);

            let mut ctx = SemanticAnalysisContext {
                max_infos: usize::MAX,
                ..Default::default()
            };

            ctx.scopes.push_front(Scope::default());

            for statement in statements {
                statement.perform_semantic_analysis(&mut ctx, false);
            }

            diagnostics.extend(ctx.infos.iter().map(|info| Diagnostic {
                range: Range {
                    start: line_index.offset_to_position(info.span.start),
                    end: line_index.offset_to_position(info.span.end),
                },
                severity: Some(match info.kind {
                    SemanticAnalysisInfoKind::Error(_) => DiagnosticSeverity::ERROR,
                }),
                source: Some("kelp-lsp".to_string()),
                message: match &info.kind {
                    SemanticAnalysisInfoKind::Error(error) => error.to_string(),
                },
                code_description: None,
                code: None,
                related_information: None,
                tags: None,
                data: None,
            }));
        }

        let mut map = self.document_map.write().await;
        map.insert(uri.clone(), DocumentState { text, line_index });

        Some(diagnostics)
    }
}

fn process_semantic_tokens(
    text: &str,
    line_index: &LineIndex,
    parser_tokens: Vec<(Span, SemanticToken)>,
) -> Vec<SemanticToken> {
    if parser_tokens.is_empty() {
        return Vec::new();
    }

    let mut semantic_tokens_from_parser = parser_tokens;
    semantic_tokens_from_parser.sort_by_key(|(span, _)| span.start);

    let mut previous_line = 0;
    let mut previous_start_utf16 = 0;
    let mut lsp_tokens = Vec::new();

    for (span, token) in semantic_tokens_from_parser {
        let start_pos = line_index.offset_to_position(span.start);

        let delta_line = start_pos.line - previous_line;
        let delta_start = if delta_line == 0 {
            start_pos.character - previous_start_utf16
        } else {
            start_pos.character
        };

        let length = text[span.into_range()].encode_utf16().count() as u32;

        lsp_tokens.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: token.token_type,
            token_modifiers_bitset: 0,
        });

        previous_line = start_pos.line;
        previous_start_utf16 = start_pos.character;
    }

    lsp_tokens
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            work_done_progress_options: WorkDoneProgressOptions::default(),
                            legend: SemanticTokensLegend {
                                token_types: LEGEND_TYPE.to_vec(),
                                token_modifiers: vec![],
                            },
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: Some(true),
                        },
                    ),
                ),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![
                        " ".to_string(),
                        "/".to_string(),
                        ".".to_string(),
                        ":".to_string(),
                        "(".to_string(),
                        "[".to_string(),
                        ",".to_string(),
                        "=".to_string(),
                        "{".to_string(),
                        "@".to_string(),
                    ]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    ..Default::default()
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                    trigger_characters: Some(vec![" ".to_string()]),
                    retrigger_characters: Some(vec![" ".to_string()]),
                }),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Kelp server initialized.")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.on_change(params.text_document.uri, params.text_document.text)
            .await;
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.on_change(
            params.text_document.uri,
            std::mem::take(&mut params.content_changes[0].text),
        )
        .await;
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        // let uri = params.text_document.uri;
        // let map = self.document_map.read().await;
        // let state = map
        //     .get(&uri)
        //     .ok_or_else(|| tower_lsp::jsonrpc::Error::invalid_params("Document not found"))?;

        // let mut input = Stream::new(&state.text);
        // input.config.semantic_tokens = true;
        // let succeeded = file(&mut input).is_some();

        // let semantic_tokens = if succeeded {
        //     input.semantic_tokens
        // } else {
        //     input.max_error.semantic_tokens
        // };

        // let lsp_tokens = process_semantic_tokens(&state.text, &state.line_index, semantic_tokens);

        // if lsp_tokens.is_empty() {
        //     Ok(None)
        // } else {
        //     Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
        //         result_id: None,
        //         data: lsp_tokens,
        //     })))
        // }

        Ok(None)
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        // let uri = params.text_document.uri;
        // let map = self.document_map.read().await;
        // let state = map
        //     .get(&uri)
        //     .ok_or_else(|| tower_lsp::jsonrpc::Error::invalid_params("Document not found"))?;

        // let (Some(start), Some(end)) = (
        //     state.line_index.position_to_offset(params.range.start),
        //     state.line_index.position_to_offset(params.range.end),
        // ) else {
        //     return Ok(None);
        // };

        // let range = Some(Span { start, end });
        // let mut input = Stream::new(&state.text);
        // input.config.semantic_tokens = true;
        // input.semantic_tokens_range = range;
        // file(&mut input);

        // let lsp_tokens =
        //     process_semantic_tokens(&state.text, &state.line_index, input.semantic_tokens);

        // if lsp_tokens.is_empty() {
        //     Ok(None)
        // } else {
        //     Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
        //         result_id: None,
        //         data: lsp_tokens,
        //     })))
        // }

        Ok(None)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        // let uri = params.text_document_position.text_document.uri;
        // let cursor_position = params.text_document_position.position;

        // let map = self.document_map.read().await;
        // let state = match map.get(&uri) {
        //     Some(s) => s,
        //     None => return Ok(None),
        // };

        // let cursor_offset = match state.line_index.position_to_offset(cursor_position) {
        //     Some(o) => o,
        //     None => return Ok(None),
        // };

        // let mut input = Stream::new(&state.text);
        // input.cursor = Some(cursor_offset);
        // file(&mut input);

        // let completion_items = input
        //     .suggestions
        //     .into_iter()
        //     .filter_map(|Suggestion { range, expected }| {
        //         match expected {
        //             Expectation::Literal(literal) => Some(literal.to_string()),
        //             Expectation::Char(char) => Some(char.to_string()),

        //             _ => None,
        //         }
        //         .map(|label| {
        //             let insert_text = match label.as_str() {
        //                 "(" => "($0)".to_string(),
        //                 "{" => "{$0}".to_string(),
        //                 "[" => "[$0]".to_string(),
        //                 "\"" => "\"$0\"".to_string(),
        //                 "'" => "'$0'".to_string(),
        //                 _ => label.clone(),
        //             };

        //             CompletionItem {
        //                 label: label.clone(),
        //                 kind: Some(CompletionItemKind::KEYWORD),
        //                 insert_text_format: Some(InsertTextFormat::SNIPPET),
        //                 text_edit: Some(CompletionTextEdit::Edit(TextEdit {
        //                     range: Range {
        //                         start: state.line_index.offset_to_position(range.start),
        //                         end: state.line_index.offset_to_position(range.start),
        //                     },
        //                     new_text: insert_text,
        //                 })),
        //                 ..Default::default()
        //             }
        //         })
        //     })
        //     .collect::<Vec<CompletionItem>>();

        // if completion_items.is_empty() {
        //     Ok(None)
        // } else {
        //     Ok(Some(CompletionResponse::Array(completion_items)))
        // }

        Ok(None)
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        // let uri = params.text_document_position_params.text_document.uri;
        // let cursor_position = params.text_document_position_params.position;

        // let map = self.document_map.read().await;
        // let state = match map.get(&uri) {
        //     Some(s) => s,
        //     None => return Ok(None),
        // };

        // let cursor_offset = match state.line_index.position_to_offset(cursor_position) {
        //     Some(o) => o,
        //     None => return Ok(None),
        // };

        // let mut input = Stream::new(&state.text);
        // input.cursor = Some(cursor_offset);
        // input.config.signatures = true;

        // file(&mut input);

        // if input.signatures.is_empty() {
        //     Ok(None)
        // } else {
        //     Ok(Some(SignatureHelp {
        //         active_signature: None,
        //         active_parameter: None,
        //         signatures: input
        //             .signatures
        //             .iter()
        //             .map(|signature| SignatureInformation {
        //                 active_parameter: Some(
        //                     signature
        //                         .active_parameter
        //                         .unwrap_or(signature.parameters.len())
        //                         as u32,
        //                 ),
        //                 label: signature.label.to_string(),
        //                 parameters: Some(
        //                     signature
        //                         .parameters
        //                         .iter()
        //                         .map(|(label, documentation)| ParameterInformation {
        //                             documentation: documentation.map(|documentation| {
        //                                 Documentation::String(documentation.to_string())
        //                             }),
        //                             label: ParameterLabel::Simple(label.to_string()),
        //                         })
        //                         .collect(),
        //                 ),
        //                 documentation: signature
        //                     .documentation
        //                     .map(|documentation| Documentation::String(documentation.to_string())),
        //             })
        //             .collect(),
        //     }))
        // }

        Ok(None)
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        document_map: RwLock::new(BTreeMap::new()),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
