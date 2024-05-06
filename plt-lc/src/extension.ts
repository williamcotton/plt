import { workspace, ExtensionContext, commands } from "vscode";

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient;

const restartClient = async () => {
  const pltServerPath = "/Users/williamcotton/Projects/plt/plt-lsp/bin/Debug/net8.0/plt-lsp.dll";
  const dotnetPath = "/Users/williamcotton/.asdf/shims/dotnet";

  // If the extension is launched in debug mode then the debug server options are used
  // Otherwise the run options are used
  const serverOptions: ServerOptions = {
    run: {
      command: dotnetPath,
      args: [pltServerPath],
      transport: TransportKind.stdio,
    },
    debug: {
      command: dotnetPath,
      args: [pltServerPath],
      transport: TransportKind.stdio,
    },
  };

  // Options to control the language client
  const clientOptions: LanguageClientOptions = {
    // Register the server for plain text documents
    documentSelector: [{ scheme: "file", language: "plt" }],
    synchronize: {},
  };

  if (!client) {
    // Create the language client and start the client.
    client = new LanguageClient(
      "plt",
      "plt Server",
      serverOptions,
      clientOptions
    );
  }

  if (client && client.isRunning()) {
    await client.restart();
  } else {
    // Start the client. This will also launch the server
    await client.start();
  }
};

export function activate(context: ExtensionContext) {
  context.subscriptions.push(
    commands.registerCommand("plt.server.restart", () => {
      // TODO: handle promise below
      restartClient();
    })
  );

  restartClient();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}