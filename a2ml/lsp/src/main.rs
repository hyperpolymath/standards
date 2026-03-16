// SPDX-License-Identifier: PMPL-1.0-or-later
//
// a2ml-lsp — Language Server Protocol server for A2ML (Attested Markup Language).
//
// Entry point: configures stdio transport and initialises the LSP backend.
// The server provides diagnostics, completions, and hover for .a2ml files.
//
// Author: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>

#![forbid(unsafe_code)]
mod completions;
mod diagnostics;
mod hover;

use dashmap::DashMap;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

/// Per-document state held by the backend.
///
/// `documents` maps a URI to its current full text so that every notification
/// (didOpen / didChange / didSave) can re-run diagnostics without asking the
/// editor for the file contents.
#[derive(Debug)]
pub struct Backend {
    /// The LSP client handle used to push diagnostics and other notifications.
    client: Client,
    /// Thread-safe map of open document URIs to their latest full text.
    documents: DashMap<String, String>,
}

impl Backend {
    /// Create a new backend bound to the given LSP client.
    fn new(client: Client) -> Self {
        Self {
            client,
            documents: DashMap::new(),
        }
    }

    /// Run all diagnostic checks on a document and publish the results.
    ///
    /// Called after every content-changing event (open, change, save).
    async fn publish_diagnostics(&self, uri: Url, text: &str) {
        let diags = diagnostics::run_all_checks(text);
        self.client
            .publish_diagnostics(uri, diags, None)
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    /// Handle the `initialize` request.
    ///
    /// Advertises our capabilities: full-document sync, completion (triggered
    /// by `@` and `[`), and hover.
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec!["@".into(), "[".into()]),
                    resolve_provider: Some(false),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "a2ml-lsp".into(),
                version: Some(env!("CARGO_PKG_VERSION").into()),
            }),
        })
    }

    /// Handle the `initialized` notification.
    ///
    /// Logs a startup message so the editor knows the server is ready.
    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "A2ML LSP server initialised")
            .await;
    }

    /// Handle the `shutdown` request (no-op; cleanup happens on drop).
    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    // ── Document synchronisation ─────────────────────────────────────

    /// A document was opened — store its text and publish initial diagnostics.
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let text = params.text_document.text.clone();
        self.documents
            .insert(uri.to_string(), text.clone());
        self.publish_diagnostics(uri, &text).await;
    }

    /// A document changed — update stored text and re-publish diagnostics.
    ///
    /// We use FULL sync so the first (and only) content change contains the
    /// entire new text.
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        if let Some(change) = params.content_changes.into_iter().next() {
            let text = change.text;
            self.documents
                .insert(uri.to_string(), text.clone());
            self.publish_diagnostics(uri, &text).await;
        }
    }

    /// A document was saved — re-publish diagnostics (may catch save-time-only
    /// checks in future).
    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        if let Some(ref entry) = self.documents.get(&uri.to_string()) {
            let text = entry.value().clone();
            self.publish_diagnostics(uri, &text).await;
        }
    }

    /// A document was closed — remove it from our cache.
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.documents
            .remove(&params.text_document.uri.to_string());
    }

    // ── Completions ──────────────────────────────────────────────────

    /// Provide context-aware completions for directives, sections, keys, and
    /// `@ref()` targets.
    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri.to_string();
        let position = params.text_document_position.position;

        let text = match self.documents.get(&uri) {
            Some(entry) => entry.value().clone(),
            None => return Ok(None),
        };

        let items = completions::provide_completions(&text, position);
        Ok(Some(CompletionResponse::Array(items)))
    }

    // ── Hover ────────────────────────────────────────────────────────

    /// Show documentation when hovering over directives or section headers.
    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;

        let text = match self.documents.get(&uri) {
            Some(entry) => entry.value().clone(),
            None => return Ok(None),
        };

        Ok(hover::provide_hover(&text, position))
    }
}

/// Application entry point.
///
/// Constructs the tower-lsp service and connects it to stdin/stdout so that
/// any LSP-capable editor can launch `a2ml-lsp --stdio` as a child process.
#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
