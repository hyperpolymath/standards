// SPDX-License-Identifier: PMPL-1.0-or-later
//
// main.rs — Entry point for the K9 LSP server
//
// Starts the Language Server Protocol server over stdio transport.
// Provides diagnostics, completions, and hover for .k9 and .k9.ncl files.
//
// Usage:
//   k9-lsp          # Start LSP server on stdin/stdout
//   k9-lsp --help   # Show usage information

#![forbid(unsafe_code)]
mod completions;
mod diagnostics;
mod hover;

use std::collections::HashMap;
use std::sync::Mutex;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

/// K9 Language Server backend.
///
/// Maintains a map of open document URIs to their current text content,
/// and provides diagnostics, completions, and hover documentation for
/// K9 Self-Validating Component files.
struct K9Backend {
    /// LSP client handle for sending notifications (diagnostics, etc.).
    client: Client,
    /// In-memory store of open document contents, keyed by URI string.
    documents: Mutex<HashMap<String, String>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for K9Backend {
    /// Called when the client initialises the server.
    ///
    /// Advertises capabilities: text document sync (full), completions,
    /// hover, and diagnostic support.
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![
                        "=".to_string(),
                        "'".to_string(),
                        "|".to_string(),
                        ".".to_string(),
                    ]),
                    resolve_provider: Some(false),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    /// Called after the client confirms initialisation.
    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "K9 LSP server initialized")
            .await;
    }

    /// Called when the client shuts down.
    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    /// Called when a document is opened — stores content and publishes diagnostics.
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        let text = params.text_document.text.clone();

        {
            let mut docs = self.documents.lock().unwrap();
            docs.insert(uri.clone(), text.clone());
        }

        let diags = diagnostics::diagnose(&text);
        self.client
            .publish_diagnostics(params.text_document.uri, diags, None)
            .await;
    }

    /// Called when a document is modified — updates content and re-publishes diagnostics.
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        // Full sync: the last content change contains the entire document.
        if let Some(change) = params.content_changes.into_iter().last() {
            let text = change.text.clone();
            {
                let mut docs = self.documents.lock().unwrap();
                docs.insert(uri, text.clone());
            }

            let diags = diagnostics::diagnose(&text);
            self.client
                .publish_diagnostics(params.text_document.uri, diags, None)
                .await;
        }
    }

    /// Called when a document is closed — removes it from the in-memory store.
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let mut docs = self.documents.lock().unwrap();
        docs.remove(&params.text_document.uri.to_string());
    }

    /// Provides context-aware completions for K9 pedigree fields, security
    /// levels, recipe names, and Nickel standard library imports.
    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri.to_string();
        let position = params.text_document_position.position;

        let docs = self.documents.lock().unwrap();
        let text = match docs.get(&uri) {
            Some(t) => t.clone(),
            None => return Ok(None),
        };

        let items = completions::complete(&text, position);
        Ok(Some(CompletionResponse::Array(items)))
    }

    /// Provides hover documentation for security levels, pedigree fields,
    /// and contract types.
    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;

        let docs = self.documents.lock().unwrap();
        let text = match docs.get(&uri) {
            Some(t) => t.clone(),
            None => return Ok(None),
        };

        Ok(hover::hover_at(&text, position))
    }
}

/// Main entry point — starts the K9 LSP server on stdio transport.
///
/// The server communicates via JSON-RPC over stdin/stdout, which is the
/// standard transport for editor integrations (VS Code, Neovim, etc.).
#[tokio::main]
async fn main() {
    // Handle --help / --version flags before starting the server.
    let args: Vec<String> = std::env::args().collect();
    if args.iter().any(|a| a == "--help" || a == "-h") {
        eprintln!("k9-lsp — Language Server Protocol server for K9 SVC files");
        eprintln!();
        eprintln!("Usage: k9-lsp");
        eprintln!("  Starts the LSP server on stdin/stdout (JSON-RPC).");
        eprintln!();
        eprintln!("Typically invoked by your editor, not directly.");
        eprintln!("See README.adoc for integration instructions.");
        std::process::exit(0);
    }
    if args.iter().any(|a| a == "--version" || a == "-V") {
        eprintln!("k9-lsp {}", env!("CARGO_PKG_VERSION"));
        std::process::exit(0);
    }

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| K9Backend {
        client,
        documents: Mutex::new(HashMap::new()),
    });

    Server::new(stdin, stdout, socket).serve(service).await;
}
