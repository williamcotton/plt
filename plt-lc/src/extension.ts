// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as path from "path";
import * as vscode from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {
  // The server is implemented in an external executable (your language server)
  const serverExecutablePath = "path/to/your/language/server/executable";

  // If your server is a Node.js project, use this:
  // const serverModule = context.asAbsolutePath(path.join('server', 'out', 'server.js'));

  // Server options define how to start and communicate with the server
  const serverOptions: ServerOptions = {
    run: { command: serverExecutablePath, transport: TransportKind.stdio },
    debug: { command: serverExecutablePath, transport: TransportKind.stdio },
  };

  // Client options define how the client will interact with the server
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "plt" }], // Replace 'plt' with your language ID
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.plt"), // Replace '*.plt' with your file extension
    },
  };

  // Create the language client and start it
  const client = new LanguageClient(
    "pltLanguageClient", // This can be any name for the client
    "PLT Language Server", // This is the name that will appear in the UI
    serverOptions,
    clientOptions
  );

  // Start the client and push the disposable to the context's subscriptions
  context.subscriptions.push(client.start());
}

// This method is called when your extension is deactivated
export function deactivate() {}
