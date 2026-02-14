use kelp_core::generate_message_error;
use kelp_core::semantic_analysis_context::{SemanticAnalysisContext, SemanticAnalysisInfoKind};
use kelp_parser::file;
use parser_rs::{
    Expectation, Suggestion, fn_parser::FnParser, parser_range::ParserRange,
    semantic_token::SemanticTokenKind, stream::Stream,
};
use std::collections::BTreeMap;
use std::time::Instant;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

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
    line_starts: Vec<usize>,
}

impl LineIndex {
    fn new(text: &str) -> Self {
        let mut line_starts = vec![0];
        line_starts.extend(text.match_indices('\n').map(|(i, _)| i + 1));
        Self { line_starts }
    }

    fn offset_to_position(&self, offset: usize, text: &str) -> Position {
        let clamped_offset = offset.min(text.len());

        let line_index = self
            .line_starts
            .binary_search(&clamped_offset)
            .unwrap_or_else(|i| i.saturating_sub(1));

        let line_start_offset = self.line_starts[line_index];

        let character_utf16 = text[line_start_offset..clamped_offset]
            .encode_utf16()
            .count();

        Position {
            line: line_index as u32,
            character: character_utf16 as u32,
        }
    }

    fn position_to_offset(&self, position: Position, text: &str) -> Option<usize> {
        let line = position.line as usize;
        if line >= self.line_starts.len() {
            return Some(text.len());
        }

        let line_start = self.line_starts[line];
        let line_end = self
            .line_starts
            .get(line + 1)
            .copied()
            .unwrap_or(text.len());

        let line_slice = &text[line_start..line_end];

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

        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }

    async fn process_text(&self, uri: &Url, text: String) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        let input = Stream::new(&text);

        let now = Instant::now();
        let result = file.parse_fully(input);
        let elapsed = now.elapsed();
        self.client
            .log_message(
                MessageType::INFO,
                format!("Parsed in (change): {:?}", elapsed),
            )
            .await;

        let line_index = LineIndex::new(&text);

        if let Err(error) = &result.result {
            diagnostics.push(Diagnostic {
                code_description: None,
                range: Range {
                    start: line_index.offset_to_position(error.span.start, &text),
                    end: line_index.offset_to_position(error.span.end, &text),
                },
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                source: Some("kelp-lsp".to_string()),
                message: generate_message_error(error),
                related_information: None,
                tags: None,
                data: None,
            });
        }

        diagnostics.extend(
            result
                .validation_errors
                .iter()
                .map(|validation_error| Diagnostic {
                    range: Range {
                        start: line_index.offset_to_position(validation_error.span.start, &text),
                        end: line_index.offset_to_position(validation_error.span.end, &text),
                    },
                    severity: Some(DiagnosticSeverity::ERROR),
                    source: Some("kelp-lsp".to_string()),
                    message: validation_error.message.to_string(),
                    code_description: None,
                    code: None,
                    related_information: None,
                    tags: None,
                    data: None,
                }),
        );

        if let Ok(statements) = result.result {
            let mut ctx = SemanticAnalysisContext {
                max_infos: 25,
                ..Default::default()
            };

            ctx.scopes.push_front(BTreeMap::new());

            for statement in statements {
                statement.perform_semantic_analysis(&mut ctx, false);
            }

            diagnostics.extend(ctx.infos.iter().map(|info| Diagnostic {
                range: Range {
                    start: line_index.offset_to_position(info.span.start, &text),
                    end: line_index.offset_to_position(info.span.end, &text),
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

        diagnostics
    }
}

fn semantic_token_kind_to_type_index(kind: SemanticTokenKind) -> u32 {
    let token_type = match kind {
        SemanticTokenKind::Namespace => SemanticTokenType::NAMESPACE,
        SemanticTokenKind::Class => SemanticTokenType::CLASS,
        SemanticTokenKind::Enum => SemanticTokenType::ENUM,
        SemanticTokenKind::Interface => SemanticTokenType::INTERFACE,
        SemanticTokenKind::Struct => SemanticTokenType::STRUCT,
        SemanticTokenKind::TypeParameter => SemanticTokenType::TYPE_PARAMETER,
        SemanticTokenKind::Type => SemanticTokenType::TYPE,
        SemanticTokenKind::Parameter => SemanticTokenType::PARAMETER,
        SemanticTokenKind::Variable => SemanticTokenType::VARIABLE,
        SemanticTokenKind::Property => SemanticTokenType::PROPERTY,
        SemanticTokenKind::EnumMember => SemanticTokenType::ENUM_MEMBER,
        SemanticTokenKind::Decorator => SemanticTokenType::DECORATOR,
        SemanticTokenKind::Event => SemanticTokenType::EVENT,
        SemanticTokenKind::Function => SemanticTokenType::FUNCTION,
        SemanticTokenKind::Method => SemanticTokenType::METHOD,
        SemanticTokenKind::Macro => SemanticTokenType::MACRO,
        SemanticTokenKind::Comment => SemanticTokenType::COMMENT,
        SemanticTokenKind::String => SemanticTokenType::STRING,
        SemanticTokenKind::Keyword => SemanticTokenType::KEYWORD,
        SemanticTokenKind::Number => SemanticTokenType::NUMBER,
        SemanticTokenKind::RegularExpression => SemanticTokenType::REGEXP,
        SemanticTokenKind::Operator => SemanticTokenType::OPERATOR,
    };

    LEGEND_TYPE.iter().position(|t| *t == token_type).unwrap() as u32
}

fn process_semantic_tokens(
    text: &str,
    line_index: &LineIndex,
    parser_tokens: Vec<parser_rs::semantic_token::SemanticToken>,
) -> Vec<SemanticToken> {
    if parser_tokens.is_empty() {
        return Vec::new();
    }

    let mut semantic_tokens_from_parser = parser_tokens;
    semantic_tokens_from_parser.sort_by_key(|t| t.range.start);

    let mut previous_line = 0;
    let mut previous_start_utf16 = 0;
    let mut lsp_tokens = Vec::new();

    for token in semantic_tokens_from_parser {
        let start_pos = line_index.offset_to_position(token.range.start, text);

        let delta_line = start_pos.line - previous_line;
        let delta_start = if delta_line == 0 {
            start_pos.character - previous_start_utf16
        } else {
            start_pos.character
        };

        let length = text[token.range.start..token.range.end]
            .encode_utf16()
            .count() as u32;

        lsp_tokens.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: semantic_token_kind_to_type_index(token.kind),
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
        let uri = params.text_document.uri;
        let map = self.document_map.read().await;
        let state = map
            .get(&uri)
            .ok_or_else(|| tower_lsp::jsonrpc::Error::invalid_params("Document not found"))?;

        let mut input = Stream::new(&state.text);
        input.config.semantic_tokens = true;
        let now = Instant::now();
        let succeeded = file(&mut input).is_some();
        let elapsed = now.elapsed();

        let semantic_tokens = if succeeded {
            input.semantic_tokens
        } else {
            input.max_error.semantic_tokens
        };

        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "[SEMANTIC TOKENS (FULL )] {:?} {}",
                    elapsed,
                    semantic_tokens.len()
                ),
            )
            .await;

        let lsp_tokens = process_semantic_tokens(&state.text, &state.line_index, semantic_tokens);

        if lsp_tokens.is_empty() {
            Ok(None)
        } else {
            Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: lsp_tokens,
            })))
        }
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        let uri = params.text_document.uri;
        let map = self.document_map.read().await;
        let state = map
            .get(&uri)
            .ok_or_else(|| tower_lsp::jsonrpc::Error::invalid_params("Document not found"))?;

        let (Some(start), Some(end)) = (
            state
                .line_index
                .position_to_offset(params.range.start, &state.text),
            state
                .line_index
                .position_to_offset(params.range.end, &state.text),
        ) else {
            return Ok(None);
        };

        let range = Some(ParserRange { start, end });
        let mut input = Stream::new(&state.text);
        input.config.semantic_tokens = true;
        input.semantic_tokens_range = range;
        let now = Instant::now();
        let _ = file(&mut input);
        let elapsed = now.elapsed();

        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "[SEMANTIC TOKENS (RANGE)] {:?} {} {:?}",
                    elapsed,
                    input.semantic_tokens.len(),
                    range
                ),
            )
            .await;

        let lsp_tokens =
            process_semantic_tokens(&state.text, &state.line_index, input.semantic_tokens);

        if lsp_tokens.is_empty() {
            Ok(None)
        } else {
            Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
                result_id: None,
                data: lsp_tokens,
            })))
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let cursor_position = params.text_document_position.position;

        let map = self.document_map.read().await;
        let state = match map.get(&uri) {
            Some(s) => s,
            None => return Ok(None),
        };

        let cursor_offset = match state
            .line_index
            .position_to_offset(cursor_position, &state.text)
        {
            Some(o) => o,
            None => return Ok(None),
        };

        let mut input = Stream::new(&state.text);
        input.cursor = Some(cursor_offset);
        let now = Instant::now();
        let _ = file(&mut input);
        let elapsed = now.elapsed();
        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "Parsed in (completion) ({}): {:?}",
                    input.suggestions.len(),
                    elapsed
                ),
            )
            .await;

        let completion_items = input
            .suggestions
            .into_iter()
            .filter_map(|Suggestion { range, expected }| {
                match expected {
                    Expectation::Literal(literal) => Some(literal.to_string()),
                    Expectation::Char(char) => Some(char.to_string()),

                    _ => None,
                }
                .map(|label| {
                    let insert_text = match label.as_str() {
                        "(" => "($0)".to_string(),
                        "{" => "{$0}".to_string(),
                        "[" => "[$0]".to_string(),
                        "\"" => "\"$0\"".to_string(),
                        "'" => "'$0'".to_string(),
                        _ => label.clone(),
                    };

                    CompletionItem {
                        label: label.clone(),
                        kind: Some(CompletionItemKind::KEYWORD),
                        insert_text_format: Some(InsertTextFormat::SNIPPET),
                        text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                            range: Range {
                                start: state
                                    .line_index
                                    .offset_to_position(range.start, &state.text),
                                end: state
                                    .line_index
                                    .offset_to_position(range.start, &state.text),
                            },
                            new_text: insert_text,
                        })),
                        ..Default::default()
                    }
                })
            })
            .collect::<Vec<CompletionItem>>();

        if completion_items.is_empty() {
            Ok(None)
        } else {
            Ok(Some(CompletionResponse::Array(completion_items)))
        }
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = params.text_document_position_params.text_document.uri;
        let cursor_position = params.text_document_position_params.position;

        let map = self.document_map.read().await;
        let state = match map.get(&uri) {
            Some(s) => s,
            None => return Ok(None),
        };

        let cursor_offset = match state
            .line_index
            .position_to_offset(cursor_position, &state.text)
        {
            Some(o) => o,
            None => return Ok(None),
        };

        let mut input = Stream::new(&state.text);
        input.cursor = Some(cursor_offset);
        input.config.signatures = true;

        let now = Instant::now();
        let _ = file(&mut input);
        let elapsed = now.elapsed();
        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "Parsed in (signature): {:?} {:?} {:?}",
                    elapsed, input.signatures, input.active_parameter
                ),
            )
            .await;

        if input.signatures.is_empty() {
            Ok(None)
        } else {
            Ok(Some(SignatureHelp {
                active_signature: None,
                active_parameter: None,
                signatures: input
                    .signatures
                    .iter()
                    .map(|signature| SignatureInformation {
                        active_parameter: Some(
                            signature
                                .active_parameter
                                .unwrap_or(signature.parameters.len())
                                as u32,
                        ),
                        label: signature.label.to_string(),
                        parameters: Some(
                            signature
                                .parameters
                                .iter()
                                .map(|(label, documentation)| ParameterInformation {
                                    documentation: documentation.map(|documentation| {
                                        Documentation::String(documentation.to_string())
                                    }),
                                    label: ParameterLabel::Simple(label.to_string()),
                                })
                                .collect(),
                        ),
                        documentation: signature
                            .documentation
                            .map(|documentation| Documentation::String(documentation.to_string())),
                    })
                    .collect(),
            }))
        }
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
