"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.deactivate = exports.activate = void 0;
const vscode_1 = require("vscode");
const node_1 = require("vscode-languageclient/node");
let client;
const restartClient = async () => {
    const pltServerPath = "/Users/williamcotton/Projects/plt/plt-lsp/bin/Debug/net8.0/plt-lsp.dll";
    const dotnetPath = "/Users/williamcotton/.asdf/shims/dotnet";
    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    const serverOptions = {
        run: {
            command: dotnetPath,
            args: [pltServerPath],
            transport: node_1.TransportKind.stdio,
        },
        debug: {
            command: dotnetPath,
            args: [pltServerPath],
            transport: node_1.TransportKind.stdio,
        },
    };
    // Options to control the language client
    const clientOptions = {
        // Register the server for plain text documents
        documentSelector: [{ scheme: "file", language: "plt" }],
        synchronize: {},
    };
    if (!client) {
        // Create the language client and start the client.
        client = new node_1.LanguageClient("plt", "plt Server", serverOptions, clientOptions);
    }
    if (client && client.isRunning()) {
        await client.restart();
    }
    else {
        // Start the client. This will also launch the server
        await client.start();
    }
};
function activate(context) {
    context.subscriptions.push(vscode_1.commands.registerCommand("plt.server.restart", () => {
        // TODO: handle promise below
        restartClient();
    }));
    restartClient();
}
exports.activate = activate;
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map