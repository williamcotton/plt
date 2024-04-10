"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.deactivate = exports.activate = void 0;
const vscode = __importStar(require("vscode"));
const node_1 = require("vscode-languageclient/node");
let client;
// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
function activate(context) {
    // The server is implemented in an external executable (your language server)
    const serverExecutablePath = "/Users/williamcotton/Projects/plt/plt-lsp/bin/Debug/net8.0/plt-lsp";
    // Server options define how to start and communicate with the server
    const serverOptions = {
        run: { command: serverExecutablePath, transport: node_1.TransportKind.stdio },
        debug: { command: serverExecutablePath, transport: node_1.TransportKind.stdio },
    };
    // Client options define how the client will interact with the server
    const clientOptions = {
        documentSelector: [{ scheme: "file", language: "plt" }], // Replace 'plt' with your language ID
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher("**/*.plt"), // Replace '*.plt' with your file extension
        },
    };
    // Create the language client and start it
    client = new node_1.LanguageClient("pltLanguageClient", // This can be any name for the client
    "PLT Language Server", // This is the name that will appear in the UI
    serverOptions, clientOptions);
    client.start();
}
exports.activate = activate;
// This method is called when your extension is deactivated
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map