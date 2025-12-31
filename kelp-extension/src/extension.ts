import * as path from "path";
import * as vscode from "vscode";
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
    const serverExe =
        process.platform === "win32" ? "kelp-lsp.exe" : "kelp-lsp";
    const serverPath = context.asAbsolutePath(path.join("server", serverExe));

    const serverOptions: ServerOptions = {
        run: { command: serverPath, transport: TransportKind.stdio },
        debug: { command: serverPath, transport: TransportKind.stdio },
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: "file", language: "kelp" }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher("**/*.kelp"),
        },
    };

    client = new LanguageClient(
        "kelp-language-server",
        "Kelp Language Server",
        serverOptions,
        clientOptions
    );

    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }

    return client.stop();
}
